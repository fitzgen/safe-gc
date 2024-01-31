#![forbid(unsafe_code)]

use std::{
    any::{Any, TypeId},
    cell::{Ref, RefCell},
    collections::HashMap,
    hash::Hash,
    rc::Rc,
    sync::atomic,
};

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

enum FreeListEntry<T> {
    Free { next_free: Option<u32> },
    Occupied { gc: Gc<T> },
}

struct GcRootSetInner<T> {
    entries: Vec<FreeListEntry<T>>,

    // The index of the first free entry in the free list.
    free: Option<u32>,
}

impl<T> Default for GcRootSetInner<T> {
    fn default() -> Self {
        Self {
            entries: Vec::default(),
            free: None,
        }
    }
}

struct GcRootSet<T> {
    inner: Rc<RefCell<GcRootSetInner<T>>>,
}

impl<T> Clone for GcRootSet<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

impl<T> Default for GcRootSet<T> {
    fn default() -> Self {
        Self {
            inner: Rc::new(RefCell::new(GcRootSetInner::<T>::default())),
        }
    }
}

impl<T> GcRootSet<T> {
    fn insert(&self, gc: Gc<T>) -> GcRoot<T> {
        let mut inner = self.inner.borrow_mut();

        let index = if let Some(index) = inner.free {
            let index = usize::try_from(index).unwrap();
            let next_free = match inner.entries[index] {
                FreeListEntry::Free { next_free } => next_free,
                FreeListEntry::Occupied { .. } => unreachable!(),
            };
            inner.free = next_free;
            inner.entries[index] = FreeListEntry::Occupied { gc };
            index
        } else {
            let index = inner.entries.len();
            inner.entries.push(FreeListEntry::Occupied { gc });
            index
        };

        GcRoot {
            roots: self.clone(),
            index,
        }
    }

    fn remove(&self, index: usize) {
        let mut inner = self.inner.borrow_mut();
        debug_assert!(matches!(
            inner.entries[index],
            FreeListEntry::Occupied { .. }
        ));
        inner.entries[index] = FreeListEntry::Free {
            next_free: inner.free,
        };
        let index = u32::try_from(index).unwrap();
        inner.free = Some(index);
    }
}

pub struct GcRoot<T> {
    roots: GcRootSet<T>,
    index: usize,
}

impl<T> Drop for GcRoot<T> {
    fn drop(&mut self) {
        self.roots.remove(self.index);
    }
}

impl<T> GcRoot<T> {
    fn as_gc(&self) -> Gc<T> {
        let inner: Ref<_> = (*self.roots.inner).borrow();
        match inner.entries[self.index] {
            FreeListEntry::Occupied { gc } => gc,
            FreeListEntry::Free { .. } => unreachable!(),
        }
    }
}

struct GcArena<T> {
    roots: GcRootSet<T>,
    elements: Vec<T>,
}

// We don't default to 0-capacity arenas because the arenas themselves are
// lazily constructed, and so by the time we are constructing an arena, we will
// always immediately push onto it.
const DEFAULT_ARENA_CAPACITY: usize = 32;

impl<T> Default for GcArena<T> {
    fn default() -> Self {
        GcArena {
            roots: GcRootSet::<T>::default(),
            elements: Vec::with_capacity(DEFAULT_ARENA_CAPACITY),
        }
    }
}

impl<T> GcArena<T> {
    #[inline]
    fn try_alloc(&mut self, heap_id: u32, value: T) -> Result<GcRoot<T>, T> {
        if self.elements.len() < self.elements.capacity() {
            let index = self.elements.len();
            let index = u32::try_from(index).unwrap();
            self.elements.push(value);
            Ok(self.roots.insert(Gc {
                heap_id,
                index,
                _phantom: std::marker::PhantomData,
            }))
        } else {
            Err(value)
        }
    }

    fn alloc_slow(&mut self, heap_id: u32, value: T) -> GcRoot<T> {
        if self.elements.len() == self.elements.capacity() {
            let additional = self.elements.len();
            self.elements.reserve(additional);
        }
        let index = self.elements.len();
        let index = u32::try_from(index).unwrap();
        self.elements.push(value);
        self.roots.insert(Gc {
            heap_id,
            index,
            _phantom: std::marker::PhantomData,
        })
    }
}

pub struct GcHeap {
    // The unique ID for this heap. Used to ensure that `Gc<T>`s are only used
    // with their associated arena. Could use branded lifetimes to avoid these
    // IDs and checks statically, but the API is gross and pushes lifetimes into
    // everything.
    id: u32,

    // A map from `type_id(T)` to `GcArena<T>`.
    arenas: HashMap<TypeId, Box<dyn Any>>,
}

impl Default for GcHeap {
    fn default() -> Self {
        GcHeap::new()
    }
}

impl<T> std::ops::Index<GcRoot<T>> for GcHeap
where
    T: Any,
{
    type Output = T;
    fn index(&self, root: GcRoot<T>) -> &Self::Output {
        &self[root.as_gc()]
    }
}

impl<T> std::ops::IndexMut<GcRoot<T>> for GcHeap
where
    T: Any,
{
    fn index_mut(&mut self, root: GcRoot<T>) -> &mut Self::Output {
        &mut self[root.as_gc()]
    }
}

impl<T> std::ops::Index<Gc<T>> for GcHeap
where
    T: Any,
{
    type Output = T;
    fn index(&self, index: Gc<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T> std::ops::IndexMut<Gc<T>> for GcHeap
where
    T: Any,
{
    fn index_mut(&mut self, gc: Gc<T>) -> &mut Self::Output {
        self.get_mut(gc)
    }
}

impl GcHeap {
    pub fn new() -> Self {
        static ID_COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(0);
        let id = ID_COUNTER.fetch_add(1, atomic::Ordering::AcqRel);
        let arenas = HashMap::default();
        Self { id, arenas }
    }

    fn arena<T>(&self) -> Option<&GcArena<T>>
    where
        T: Any,
    {
        let arena = self.arenas.get(&TypeId::of::<T>())?;
        Some(arena.downcast_ref().unwrap())
    }

    fn arena_mut<T>(&mut self) -> Option<&mut GcArena<T>>
    where
        T: Any,
    {
        let arena = self.arenas.get_mut(&TypeId::of::<T>())?;
        Some(arena.downcast_mut().unwrap())
    }

    fn ensure_arena<T>(&mut self) -> &mut GcArena<T>
    where
        T: Any,
    {
        self.arenas
            .entry(TypeId::of::<T>())
            .or_insert_with(|| Box::new(GcArena::<T>::default()) as Box<dyn Any>)
            .downcast_mut()
            .unwrap()
    }

    pub fn alloc<T>(&mut self, value: T) -> GcRoot<T>
    where
        T: Any,
    {
        let heap_id = self.id;
        let arena = self.ensure_arena::<T>();
        match arena.try_alloc(heap_id, value) {
            Ok(root) => root,
            Err(value) => self.alloc_slow(value),
        }
    }

    fn alloc_slow<T>(&mut self, value: T) -> GcRoot<T>
    where
        T: Any,
    {
        self.gc();
        let heap_id = self.id;
        let arena = self.ensure_arena::<T>();
        arena.alloc_slow(heap_id, value)
    }

    pub fn get<T>(&self, gc: Gc<T>) -> &T
    where
        T: Any,
    {
        assert_eq!(self.id, gc.heap_id);
        let arena = self.arena::<T>().unwrap();
        let index = usize::try_from(gc.index).unwrap();
        &arena.elements[index]
    }

    pub fn get_mut<T>(&mut self, gc: Gc<T>) -> &mut T
    where
        T: Any,
    {
        assert_eq!(self.id, gc.heap_id);
        let arena = self.arena_mut::<T>().unwrap();
        let index = usize::try_from(gc.index).unwrap();
        &mut arena.elements[index]
    }

    pub fn gc(&mut self) {
        todo!()
    }
}
