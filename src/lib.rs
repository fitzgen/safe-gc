#![forbid(unsafe_code)]

mod free_list;
mod mark_bits;

use free_list::FreeList;
use mark_bits::MarkBits;
use std::{
    any::{Any, TypeId},
    cell::RefCell,
    collections::HashMap,
    hash::Hash,
    rc::Rc,
    sync::atomic,
};

pub trait Trace: Any {
    fn trace(&self, collector: &mut Collector);
}

pub struct Gc<T> {
    heap_id: u32,
    index: u32,
    _phantom: std::marker::PhantomData<*mut T>,
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Gc<T> {}

impl<T> Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.heap_id.hash(state);
        self.index.hash(state);
    }
}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.heap_id == other.heap_id && self.index == other.index
    }
}

impl<T> Eq for Gc<T> {}

struct RootSet<T> {
    inner: Rc<RefCell<FreeList<Gc<T>>>>,
}

impl<T> Clone for RootSet<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

impl<T> Default for RootSet<T> {
    fn default() -> Self {
        Self {
            inner: Rc::new(RefCell::new(FreeList::<Gc<T>>::default())),
        }
    }
}

impl<T> RootSet<T>
where
    T: Trace,
{
    fn insert(&self, gc: Gc<T>) -> Root<T> {
        let mut inner = self.inner.borrow_mut();
        let index = inner.alloc(gc);
        Root {
            roots: self.clone(),
            index,
        }
    }

    fn remove(&self, index: u32) {
        let mut inner = self.inner.borrow_mut();
        inner.dealloc(index);
    }

    fn trace(&self, collector: &mut Collector) {
        let inner = self.inner.borrow();
        for (_, gc) in inner.iter() {
            collector.edge(*gc);
        }
    }
}

pub struct Root<T>
where
    T: Trace,
{
    roots: RootSet<T>,
    index: u32,
}

// TODO: impl clone

impl<T> Drop for Root<T>
where
    T: Trace,
{
    fn drop(&mut self) {
        self.roots.remove(self.index);
    }
}

impl<T> Root<T>
where
    T: Trace,
{
    pub fn unrooted(&self) -> Gc<T> {
        let inner = (*self.roots.inner).borrow();
        *inner.get(self.index)
    }
}

struct Arena<T> {
    roots: RootSet<T>,
    elements: FreeList<T>,
}

// We don't default to 0-capacity arenas because the arenas themselves are
// lazily constructed, and so by the time we are constructing an arena, we will
// always immediately push onto it.
const DEFAULT_ARENA_CAPACITY: usize = 32;

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Arena {
            roots: RootSet::<T>::default(),
            elements: FreeList::with_capacity(DEFAULT_ARENA_CAPACITY),
        }
    }
}

impl<T> Arena<T>
where
    T: Trace,
{
    #[inline]
    fn try_alloc(&mut self, heap_id: u32, value: T) -> Result<Root<T>, T> {
        let index = self.elements.try_alloc(value)?;
        Ok(self.roots.insert(Gc {
            heap_id,
            index,
            _phantom: std::marker::PhantomData,
        }))
    }

    fn alloc_slow(&mut self, heap_id: u32, value: T) -> Root<T> {
        if self.elements.len() == self.elements.capacity() {
            let additional = self.elements.len();
            self.elements.reserve(additional);
        }
        let index = self.elements.try_alloc(value).ok().unwrap();
        self.roots.insert(Gc {
            heap_id,
            index,
            _phantom: std::marker::PhantomData,
        })
    }
}

trait ArenaObject: Any {
    fn as_any(&self) -> &dyn Any;

    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn trace_roots(&self, collector: &mut Collector);

    fn trace_one(&mut self, index: u32, collector: &mut Collector);

    fn capacity(&self) -> usize;

    fn sweep(&mut self, mark_bits: &MarkBits);
}

impl<T> ArenaObject for Arena<T>
where
    T: Trace,
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn trace_roots(&self, collector: &mut Collector) {
        self.roots.trace(collector);
    }

    fn trace_one(&mut self, index: u32, collector: &mut Collector) {
        self.elements.get(index).trace(collector);
    }

    fn capacity(&self) -> usize {
        self.elements.capacity()
    }

    fn sweep(&mut self, mark_bits: &MarkBits) {
        let capacity = self.elements.capacity();
        let capacity = u32::try_from(capacity).unwrap();
        for index in 0..capacity {
            if !mark_bits.get(index) {
                self.elements.dealloc(index);
            }
        }
    }
}

#[derive(Default)]
pub struct Collector {
    mark_stacks: HashMap<TypeId, Vec<u32>>,
    mark_bits: HashMap<TypeId, MarkBits>,
}

impl Collector {
    pub fn edge<T>(&mut self, to: Gc<T>)
    where
        T: Trace,
    {
        let ty = TypeId::of::<T>();
        let mark_bits = self.mark_bits.get_mut(&ty).unwrap();
        if mark_bits.set(to.index) {
            return;
        }
        let mark_stack = self.mark_stacks.entry(ty).or_default();
        mark_stack.push(to.index);
    }

    fn next_non_empty_mark_stack(&self) -> Option<TypeId> {
        self.mark_stacks.iter().find_map(
            |(ty, stack)| {
                if stack.is_empty() {
                    None
                } else {
                    Some(*ty)
                }
            },
        )
    }

    fn pop_mark_stack(&mut self, type_id: TypeId) -> Option<u32> {
        self.mark_stacks.get_mut(&type_id).unwrap().pop()
    }
}

pub struct Heap {
    // The unique ID for this heap. Used to ensure that `Gc<T>`s are only used
    // with their associated arena. Could use branded lifetimes to avoid these
    // IDs and checks statically, but the API is gross and pushes lifetimes into
    // everything.
    id: u32,

    // A map from `type_id(T)` to `GcArena<T>`.
    arenas: HashMap<TypeId, Box<dyn ArenaObject>>,

    collector: Collector,
}

impl Default for Heap {
    fn default() -> Self {
        Heap::new()
    }
}

impl<T> std::ops::Index<Root<T>> for Heap
where
    T: Trace,
{
    type Output = T;
    fn index(&self, root: Root<T>) -> &Self::Output {
        &self[root.unrooted()]
    }
}

impl<T> std::ops::IndexMut<Root<T>> for Heap
where
    T: Trace,
{
    fn index_mut(&mut self, root: Root<T>) -> &mut Self::Output {
        &mut self[root.unrooted()]
    }
}

impl<'a, T> std::ops::Index<&'a Root<T>> for Heap
where
    T: Trace,
{
    type Output = T;
    fn index(&self, root: &'a Root<T>) -> &Self::Output {
        &self[root.unrooted()]
    }
}

impl<'a, T> std::ops::IndexMut<&'a Root<T>> for Heap
where
    T: Trace,
{
    fn index_mut(&mut self, root: &'a Root<T>) -> &mut Self::Output {
        &mut self[root.unrooted()]
    }
}

impl<T> std::ops::Index<Gc<T>> for Heap
where
    T: Trace,
{
    type Output = T;
    fn index(&self, index: Gc<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T> std::ops::IndexMut<Gc<T>> for Heap
where
    T: Trace,
{
    fn index_mut(&mut self, gc: Gc<T>) -> &mut Self::Output {
        self.get_mut(gc)
    }
}

impl Heap {
    pub fn new() -> Self {
        Self {
            id: Self::next_id(),
            arenas: HashMap::default(),
            collector: Collector::default(),
        }
    }

    fn next_id() -> u32 {
        static ID_COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(0);
        ID_COUNTER.fetch_add(1, atomic::Ordering::AcqRel)
    }

    fn arena<T>(&self) -> Option<&Arena<T>>
    where
        T: Trace,
    {
        let arena = self.arenas.get(&TypeId::of::<T>())?;
        Some(arena.as_any().downcast_ref().unwrap())
    }

    fn arena_mut<T>(&mut self) -> Option<&mut Arena<T>>
    where
        T: Trace,
    {
        let arena = self.arenas.get_mut(&TypeId::of::<T>())?;
        Some(arena.as_any_mut().downcast_mut().unwrap())
    }

    fn ensure_arena<T>(&mut self) -> &mut Arena<T>
    where
        T: Trace,
    {
        self.arenas
            .entry(TypeId::of::<T>())
            .or_insert_with(|| Box::new(Arena::<T>::default()) as _)
            .as_any_mut()
            .downcast_mut()
            .unwrap()
    }

    pub fn alloc<T>(&mut self, value: T) -> Root<T>
    where
        T: Trace,
    {
        let heap_id = self.id;
        let arena = self.ensure_arena::<T>();
        match arena.try_alloc(heap_id, value) {
            Ok(root) => root,
            Err(value) => self.alloc_slow(value),
        }
    }

    fn alloc_slow<T>(&mut self, value: T) -> Root<T>
    where
        T: Trace,
    {
        self.gc();
        let heap_id = self.id;
        let arena = self.ensure_arena::<T>();
        arena.alloc_slow(heap_id, value)
    }

    pub fn get<T>(&self, gc: Gc<T>) -> &T
    where
        T: Trace,
    {
        assert_eq!(self.id, gc.heap_id);
        let arena = self.arena::<T>().unwrap();
        arena.elements.get(gc.index)
    }

    pub fn get_mut<T>(&mut self, gc: Gc<T>) -> &mut T
    where
        T: Trace,
    {
        assert_eq!(self.id, gc.heap_id);
        let arena = self.arena_mut::<T>().unwrap();
        arena.elements.get_mut(gc.index)
    }

    pub fn gc(&mut self) {
        debug_assert!(self.collector.mark_stacks.values().all(|s| s.is_empty()));

        // Reset/pre-allocate the mark bits.
        for (ty, arena) in &self.arenas {
            self.collector
                .mark_bits
                .entry(*ty)
                .or_default()
                .reset(arena.capacity());
        }

        // Mark all roots.
        for arena in self.arenas.values() {
            arena.trace_roots(&mut self.collector);
        }

        // Mark everything transitively reachable from the roots.
        //
        // NB: We have a two-level fixed-point loop to avoid checking if every
        // mark stack is non-empty on every iteration of the hottest, inner-most
        // loop.
        while let Some(type_id) = self.collector.next_non_empty_mark_stack() {
            while let Some(index) = self.collector.pop_mark_stack(type_id) {
                self.arenas
                    .get_mut(&type_id)
                    .unwrap()
                    .trace_one(index, &mut self.collector);
            }
        }

        // Sweep.
        for (ty, arena) in &mut self.arenas {
            let mark_bits = &self.collector.mark_bits[ty];
            arena.sweep(mark_bits);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn object_safety() {
        fn _trace(_: &dyn Trace) {}
        fn _arena_object(_: &dyn ArenaObject) {}
    }
}
