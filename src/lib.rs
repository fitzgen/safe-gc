#![doc = include_str!("../README.md")]
#![forbid(unsafe_code)]
#![deny(missing_docs)]

mod free_list;
mod mark_bits;

use free_list::FreeList;
use mark_bits::MarkBits;
use std::{
    any::{Any, TypeId},
    cell::RefCell,
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    rc::Rc,
    sync::atomic,
};

/// Report references to other GC-managed objects to the collector.
///
/// This trait must be implemented by all types that are allocated within a
/// [`Heap`].
///
/// Failure to enumerate all edges in an instance will result in wacky -- but
/// still safe! -- behavior: panics when attempting to access a collected
/// object, accesses to a "wrong" object that's been allocated in place of the
/// old object, etc...
///
/// # Example
///
/// ```
/// use safe_gc::{Collector, Gc, Trace};
///
/// struct List<T: Trace> {
///     value: Gc<T>,
///     prev: Option<Gc<List<T>>>,
///     next: Option<Gc<List<T>>>,
/// }
///
/// impl<T: Trace> Trace for List<T> {
///     fn trace(&self, collector: &mut Collector) {
///         collector.edge(self.value);
///         if let Some(prev) = self.prev {
///             collector.edge(prev);
///         }
///         if let Some(next) = self.next {
///             collector.edge(next);
///         }
///     }
/// }
/// ```
pub trait Trace: Any {
    /// Call `collector.edge(gc)` for each `Gc<T>` reference within `self`.
    fn trace(&self, collector: &mut Collector);
}

/// A reference to a garbage-collected `T`.
///
/// `Gc<T>` should be used when:
///
/// * Referencing other GC-managed objects from within a GC-managed object's
///   type definition.
///
/// * Traversing or mutating GC-managed objects when you know a garbage
///   collection cannot happen.
///
/// A `Gc<T>` does *not* root the referenced `T` or keep it alive across garbage
/// collections. (The [`Root<T>`][crate::Root] type does that.) Therefore,
/// `Gc<T>` should *not* be used to hold onto GC references across any operation
/// that could trigger a garbage collection.
///
/// # Example: Referencing Other GC-Managed Objects Within a GC-Managed Object
///
/// ```
/// use safe_gc::{Collector, Gc, Trace};
///
/// struct Tree<T: Trace> {
///     // A non-nullable reference to a GC-managed `T`.
///     value: Gc<T>,
///
///     // Nullable references to parent, left, and right tree nodes.
///     parent: Option<Gc<Tree<T>>>,
///     left: Option<Gc<Tree<T>>>,
///     right: Option<Gc<Tree<T>>>,
/// }
///
/// impl<T: Trace> Trace for Tree<T> {
///     fn trace(&self, collector: &mut Collector) {
///         // Report each of the `Gc<T>`s referenced from within `self` to the
///         // collector. See the `Trace` docs for more details.
///         collector.edge(self.value);
///         if let Some(parent) = self.parent {
///             collector.edge(parent);
///         }
///         if let Some(left) = self.left {
///             collector.edge(left);
///         }
///         if let Some(right) = self.right {
///             collector.edge(right);
///         }
///     }
/// }
/// ```
///
/// # Example: Accessing a `Gc<T>`'s referenced `T`
///
/// ```
/// use safe_gc::{Gc, Heap, Trace};
///
/// struct Node {
///     value: u32,
///     tail: Option<Gc<Node>>,
/// }
///
/// impl Trace for Node {
///     // ...
/// #   fn trace(&self, _: &mut safe_gc::Collector) {}
/// }
///
/// let mut heap = Heap::new();
///
/// let a = heap.alloc(Node { value: 36, tail: None });
/// let b = heap.alloc(Node { value: 42, tail: Some(a.into()) });
/// let c = heap.alloc(Node { value: 99, tail: Some(b.clone().into()) });
///
/// // Read `(*c).tail`.
/// let c_tail = heap[&c].tail;
/// assert_eq!(c_tail, Some(b.into()));
///
/// // Write `(*c).tail = None`.
/// heap[&c].tail = None;
/// ```
///
/// # Example: Downgrading a `Root<T>` into a `Gc<T>`
///
/// The [`Heap::alloc`] method returns rooted references, but to store those
/// references into the field of a GC-managed object, you'll need to unroot the
/// reference with [`Root<T>::unrooted`][crate::Root::unrooted]. (You can also
/// use `root.into()` or `Gc::from(root)`.)
///
/// ```
/// use safe_gc::{Gc, Heap, Root, Trace};
///
/// struct Cat {
///     siblings: Vec<Gc<Cat>>,
/// }
///
/// impl Trace for Cat {
///     // ...
/// #   fn trace(&self, _: &mut safe_gc::Collector) {}
/// }
///
/// let mut heap = Heap::new();
///
/// let momo: Root<Cat> = heap.alloc(Cat { siblings: vec![] });
/// let juno: Root<Cat> = heap.alloc(Cat { siblings: vec![] });
///
/// // Add `momo` and `juno` to each other's siblings vectors. This requires
/// // downgrading the `Root<Cat>`s to `Gc<Cat>`s via the `unrooted` method.
/// heap[&momo].siblings.push(juno.unrooted());
/// heap[&juno].siblings.push(momo.unrooted());
/// ```
///
/// # Example: Upgrading a `Gc<T>` into a `Root<T>`
///
/// You can upgrade a `Gc<T>` into a [`Root<T>`][crate::Root] via the
/// [`Heap::root`] method, so that you can hold references to GC-objects across
/// operations that can potentially trigger garbage collections.
///
/// See the docs for [`Heap::root`] for more details and an example.
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

impl<T> Debug for Gc<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("Gc<{}>", std::any::type_name::<T>()))
            .field("heap_id", &self.heap_id)
            .field("index", &self.index)
            .finish()
    }
}

impl<T> Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.heap_id.hash(state);
        self.index.hash(state);
    }
}

impl<T> PartialEq<Self> for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.heap_id == other.heap_id && self.index == other.index
    }
}

impl<T> PartialEq<Root<T>> for Gc<T>
where
    T: Trace,
{
    fn eq(&self, other: &Root<T>) -> bool {
        *self == other.unrooted()
    }
}

impl<T> Eq for Gc<T> {}

impl<T> From<Root<T>> for Gc<T>
where
    T: Trace,
{
    fn from(root: Root<T>) -> Self {
        root.unrooted()
    }
}

impl<'a, T> From<&'a Root<T>> for Gc<T>
where
    T: Trace,
{
    fn from(root: &'a Root<T>) -> Self {
        root.unrooted()
    }
}

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

/// A rooted reference to a GC-managed `T`.
///
/// `Root<T>`s prevent their referenced `T` from being reclaimed during garbage
/// collections. This makes them suitable for holding references to GC-managed
/// objects across operations that can trigger GCs.
///
/// `Root<T>`s are *not* suitable for referencing other GC-manged objects within
/// the definition of a GC-managed object. Doing this will effectively leak
/// everything transitively referenced from the `Root<T>`. Instead, use
/// [`Gc<T>`][crate::Gc] for references to other GC-managed objects from within
/// a GC-managed object.
///
/// See also the docs for [`Gc<T>`][crate::Gc] for more examples of converting
/// between `Root<T>` and `Gc<T>` and when you want to use which type.
///
/// # Example: Creating a `Root<T>` via Allocation
///
/// ```
/// use safe_gc::{Gc, Heap, Root, Trace};
///
/// struct Node {
///     value: u32,
///     tail: Option<Gc<Node>>,
/// }
///
/// impl Trace for Node {
///     // ...
/// #   fn trace(&self, _: &mut safe_gc::Collector) {}
/// }
///
/// let mut heap = Heap::new();
///
/// // Allocating a new GC object in a heap returns a `Root<T>` reference.
/// let node: Root<Node> = heap.alloc(Node { value: 1234, tail: None });
/// ```
pub struct Root<T>
where
    T: Trace,
{
    roots: RootSet<T>,
    index: u32,
}

impl<T> Clone for Root<T>
where
    T: Trace,
{
    fn clone(&self) -> Self {
        self.roots.insert(self.unrooted())
    }
}

impl<T> PartialEq<Root<T>> for Root<T>
where
    T: Trace,
{
    fn eq(&self, other: &Root<T>) -> bool {
        self.unrooted() == other.unrooted()
    }
}

impl<T> PartialEq<Gc<T>> for Root<T>
where
    T: Trace,
{
    fn eq(&self, other: &Gc<T>) -> bool {
        self.unrooted() == *other
    }
}

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
    /// Get an unrooted [`Gc<T>`][crate::Gc] reference pointing to the same `T`
    /// that this `Root<T>` points to.
    ///
    /// See also the docs for [`Gc<T>`][crate::Gc] for more examples of
    /// converting between `Root<T>` and `Gc<T>` and when you want to use which
    /// type.
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
        Ok(self.root(Gc {
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
        self.root(Gc {
            heap_id,
            index,
            _phantom: std::marker::PhantomData,
        })
    }

    #[inline]
    fn root(&self, gc: Gc<T>) -> Root<T> {
        self.roots.insert(gc)
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

/// The garbage collector for a heap.
///
/// GC-managed objects should report all of their references to other GC-managed
/// objects (aka "edges") to the collector in their [`Trace`] implementations.
///
/// See the docs for [`Trace`] for more information.
//
// This type is only exposed to users so they can report edges, but internally
// this does a bit more than that:
//
// * It maintains the mark stack work lists that contain all the GC objects
//   we've seen but have not yet finished processing.
//
// * It maintains the mark bits for all GC objects in the heap, which keep track
//   of which GC objects we have and have not seen while tracing the live set.
pub struct Collector {
    heap_id: u32,
    mark_stacks: HashMap<TypeId, Vec<u32>>,
    mark_bits: HashMap<TypeId, MarkBits>,
}

impl Collector {
    fn new(heap_id: u32) -> Self {
        Self {
            heap_id,
            mark_stacks: HashMap::default(),
            mark_bits: HashMap::default(),
        }
    }

    /// Report a reference to another GC-managed object (aka an "edge" in the
    /// heap graph).
    ///
    /// See the docs for [`Trace`] for more information.
    ///
    /// Panics when given cross-heap edges. See the "Cross-`Heap` GC References"
    /// section of [`Heap`]'s documentation for details on cross-heap edges.
    pub fn edge<T>(&mut self, to: Gc<T>)
    where
        T: Trace,
    {
        assert_eq!(to.heap_id, self.heap_id);
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

/// A collection of GC-managed objects.
///
/// A `Heap` is a collection of GC-managed objects that can all reference each
/// other, and garbage collection is performed at the `Heap` granularity. The
/// smaller the `Heap`, the less overhead imposed by garbage collecting it. The
/// larger the `Heap`, the more overhead is imposed.
///
/// There are no restrictions on the shape of references between GC-managed
/// objects within a `Heap`: references may form arbitrary cycles and there is
/// no imposed ownership hierarchy.
///
/// ## Allocating Objects
///
/// You can allocate objects with the [`Heap::alloc`] method.
///
/// You can allocate instances of any number of heterogeneous types of
/// GC-managed objects within a `Heap`: they may, for example, contain both
/// `Cons` objects and `Tree` objects. `Heap`s are *not* constrained to only
/// instances of a single, uniform `T` type of GC objects.
///
/// All types allocated within a heap must, however, implement the [`Trace`]
/// trait. See the [`Trace`] trait's docs for more details.
///
/// ```
/// use safe_gc::{Gc, Heap, Trace};
///
/// struct Tree<T: Trace> {
///     value: Gc<T>,
///     parent: Option<Gc<Tree<T>>>,
///     left: Option<Gc<Tree<T>>>,
///     right: Option<Gc<Tree<T>>>,
/// }
///
/// impl<T: Trace> Trace for Tree<T> {
///     // See the docs for `Trace` for details...
/// #   fn trace(&self, _: &mut safe_gc::Collector) {}
/// }
///
/// struct Cat {
///     cuteness: u32,
///     cat_tree: Option<Gc<Tree<Cat>>>,
/// }
///
/// impl Trace for Cat {
///     // See the docs for `Trace` for details...
/// #   fn trace(&self, _: &mut safe_gc::Collector) {}
/// }
///
/// let mut heap = Heap::new();
///
/// // Allocate a cat within the heap!
/// let goomba = heap.alloc(Cat {
///     cuteness: u32::MAX,
///     cat_tree: None,
/// });
///
/// // Also allocate a tree within the heap!
/// let tree = heap.alloc(Tree {
///     value: goomba.unrooted(),
///     parent: None,
///     left: None,
///     right: None,
/// });
///
/// // Create a cycle: the `tree` already references `goomba`, but now
/// // `goomba` references the `tree` as well.
/// heap[goomba].cat_tree = Some(tree.into());
/// ```
///
/// ## Accessing Allocating Objects
///
/// Rather than dereferencing pointers to allocated GC objects directly, you
/// must use one of two types ([`Gc<T>`][crate::Gc] or [`Root<T>`][crate::Root])
/// to index into the `Heap` to access the referenced `T` object. This enables
/// `safe-gc`'s lack of `unsafe` code and allows the implementation to follow
/// Rust's ownership and borrowing discipline.
///
/// Given a [`Gc<T>`][crate::Gc] or [`Root<T>`][crate::Root], you can use the
/// [`Heap::get`] method to get an `&T`. Similarly, you can use the
/// [`Heap::get_mut`] method to get an `&mut T`. As convenient syntactic sugar,
/// `Heap` also implements [`std::ops::Index`] and [`std::ops::IndexMut`] as
/// aliases for `get` and `get_mut` respectively.
///
/// The [`Gc<T>`][crate::Gc] index type is an unrooted reference, suitable for
/// defining references to other GC-managed types within a GC-managed type's
/// definition.
///
/// The [`Root<T>`][crate::Root] index type is a rooted reference, prevents its
/// referent from being reclaimed during garbage collections, and is suitable
/// for holding GC-managed objects alive from outside of the `Heap`.
///
/// See the docs for [`Gc<T>`][crate::Gc] and [`Root<T>`][crate::Root] for more
/// details, how to convert between them, and other examples.
///
/// ```
/// use safe_gc::{Heap, Trace};
///
/// struct Point(u32, u32);
///
/// impl Trace for Point {
///     // ...
/// #   fn trace(&self, _: &mut safe_gc::Collector) {}
/// }
///
/// let mut heap = Heap::new();
///
/// let p = heap.alloc(Point(42, 36));
///
/// // Read data from an object in the heap.
/// let p0 = heap[&p].0;
/// assert_eq!(p0, 42);
///
/// // Write data to an object in the heap.
/// heap[&p].1 = 5;
/// ```
///
/// ## When Can Garbage Collections Happen?
///
/// There are two ways to trigger garbage collection:
///
/// 1. When the [`Heap::gc`] method is explicitly invoked.
///
/// 2. When allocating an object in the heap with [`Heap::alloc`].
///
/// Note that both of those methods require an `&mut self`, so they cannot be
/// invoked when there are shared `&Heap` borrows. Therefore, when there are
/// shared `&Heap` borrows, you may freely use [`Gc<T>`][crate::Gc] instead of
/// [`Root<T>`][crate::Root] to hold references to objects in the heap without
/// fear of the GC collecting the objects out from under your feet. Traversing
/// deeply nested structures will have more overhead when using
/// [`Root<T>`][crate::Root] than when using [`Gc<T>`][crate::Gc] because
/// [`Root<T>`][crate::Root] must maintain its associated entry in the heap's
/// root set.
///
/// ## Cross-`Heap` GC References
///
/// Typically, GC objects will only reference other GC objects that are within
/// the same `Heap`. In fact, edges reported to the [`Collector`] *must* point
/// to objects within the same `Heap` as the object being traced or else
/// [`Collector::edge`] will raise a panic.
///
/// However, you can create cross-heap edges with [`Root<T>`][crate::Root], but
/// it requires some care. While a [`Root<T>`][crate::Root] pointing to an
/// object in the same `Heap` will make all transitively referenced objects
/// unreclaimable, a [`Root<T>`][crate::Root] pointing to an object in another
/// heap will not indefinitely leak memory, provided you do not create *cycles*
/// across `Heap`s. To fully collect all garbage across all `Heap`s, you will
/// need to run GC on each `Heap` either
///
/// * in topological order of cross-`Heap` edges, if you statically know that
///   order in your application, or
///
/// * in a fixed point loop, if you do not statically know that order.
///
/// However, if you don't statically know that topological order, it is
/// recommended that you don't create cross-`Heap` edges; you will likely
/// accidentally create cycles and leak memory. Instead, simply put everything
/// in the same `Heap`.
///
/// Collecting cycles across `Heap`s would require a global, cross-`Heap`
/// collector, and is not a goal of this crate. If you do choose to create
/// cross-`Heap` references, the responsibility of avoiding cross-`Heap` cycles
/// is yours.
pub struct Heap {
    // The unique ID for this heap. Used to ensure that `Gc<T>`s are only used
    // with their associated arena. Could use branded lifetimes to avoid these
    // IDs and checks statically, but the API is gross and pushes lifetimes into
    // everything.
    id: u32,

    // A map from `type_id(T)` to `Arena<T>`.
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
    /// Construct a new `Heap`.
    ///
    /// # Example
    ///
    /// ```
    /// use safe_gc::Heap;
    ///
    /// let heap = Heap::new();
    /// ```
    #[inline]
    pub fn new() -> Self {
        let id = Self::next_id();
        Self {
            id,
            arenas: HashMap::default(),
            collector: Collector::new(id),
        }
    }

    #[inline]
    fn next_id() -> u32 {
        static ID_COUNTER: atomic::AtomicU32 = atomic::AtomicU32::new(0);
        ID_COUNTER.fetch_add(1, atomic::Ordering::AcqRel)
    }

    #[inline]
    fn arena<T>(&self) -> Option<&Arena<T>>
    where
        T: Trace,
    {
        let arena = self.arenas.get(&TypeId::of::<T>())?;
        Some(arena.as_any().downcast_ref().unwrap())
    }

    #[inline]
    fn arena_mut<T>(&mut self) -> Option<&mut Arena<T>>
    where
        T: Trace,
    {
        let arena = self.arenas.get_mut(&TypeId::of::<T>())?;
        Some(arena.as_any_mut().downcast_mut().unwrap())
    }

    #[inline]
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

    /// Allocate an object in the heap.
    ///
    /// # Example
    ///
    /// ```
    /// use safe_gc::{Gc, Heap, Trace};
    ///
    /// struct List {
    ///     value: u32,
    ///     prev: Option<Gc<List>>,
    ///     next: Option<Gc<List>>,
    /// }
    ///
    /// impl Trace for List {
    ///     // See the docs for `Trace` for details...
    /// #   fn trace(&self, _: &mut safe_gc::Collector) {}
    /// }
    ///
    /// let mut heap = Heap::new();
    ///
    /// // Allocate an object in the heap.
    /// let list = heap.alloc(List {
    ///     value: 10,
    ///     prev: None,
    ///     next: None,
    /// });
    /// ```
    #[inline]
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

    #[inline(never)]
    fn alloc_slow<T>(&mut self, value: T) -> Root<T>
    where
        T: Trace,
    {
        // TODO: need to temporarily root `value` across this GC so that its
        // edges don't get collected.
        self.gc();
        let heap_id = self.id;
        let arena = self.ensure_arena::<T>();
        arena.alloc_slow(heap_id, value)
    }

    /// Get a shared reference to an allocated object in the heap.
    ///
    /// You can also use [`std::ops::Index`] to access objects in the heap.
    ///
    /// # Example
    ///
    /// ```
    /// use safe_gc::{Gc, Heap, Trace};
    ///
    /// struct List {
    ///     value: u32,
    ///     prev: Option<Gc<List>>,
    ///     next: Option<Gc<List>>,
    /// }
    ///
    /// impl Trace for List {
    ///     // See the docs for `Trace` for details...
    /// #   fn trace(&self, _: &mut safe_gc::Collector) {}
    /// }
    ///
    /// let mut heap = Heap::new();
    ///
    /// // Allocate an object in the heap.
    /// let list = heap.alloc(List {
    ///     value: 10,
    ///     prev: None,
    ///     next: None,
    /// });
    ///
    /// // Access an allocated object in the heap.
    /// let value = heap.get(list).value;
    /// assert_eq!(value, 10);
    /// ```
    #[inline]
    pub fn get<T>(&self, gc: impl Into<Gc<T>>) -> &T
    where
        T: Trace,
    {
        let gc = gc.into();
        assert_eq!(self.id, gc.heap_id);
        let arena = self.arena::<T>().unwrap();
        arena.elements.get(gc.index)
    }

    /// Get a shared reference to an allocated object in the heap.
    ///
    /// You can also use [`std::ops::Index`] to access objects in the heap.
    ///
    /// # Example
    ///
    /// ```
    /// use safe_gc::{Gc, Heap, Trace};
    ///
    /// struct List {
    ///     value: u32,
    ///     prev: Option<Gc<List>>,
    ///     next: Option<Gc<List>>,
    /// }
    ///
    /// impl Trace for List {
    ///     // See the docs for `Trace` for details...
    /// #   fn trace(&self, _: &mut safe_gc::Collector) {}
    /// }
    ///
    /// let mut heap = Heap::new();
    ///
    /// // Allocate an object in the heap.
    /// let list = heap.alloc(List {
    ///     value: 10,
    ///     prev: None,
    ///     next: None,
    /// });
    ///
    /// // Mutate an allocated object in the heap.
    /// heap.get_mut(&list).value += 1;
    /// assert_eq!(heap[list].value, 11);
    /// ```
    #[inline]
    pub fn get_mut<T>(&mut self, gc: impl Into<Gc<T>>) -> &mut T
    where
        T: Trace,
    {
        let gc = gc.into();
        assert_eq!(self.id, gc.heap_id);
        let arena = self.arena_mut::<T>().unwrap();
        arena.elements.get_mut(gc.index)
    }

    /// Root a reference to a GC object.
    ///
    /// This method upgrades a [`Gc<T>`][crate::Gc] into a
    /// [`Root<T>`][crate::Root]. This is useful for holding references to
    /// GC-managed objects across operations that can potentially trigger
    /// garbage collections, making sure that the collector doesn't reclaim them
    /// from under your feed.
    ///
    /// # Example
    ///
    /// ```
    /// use safe_gc::{Gc, Heap, Root, Trace};
    ///
    /// struct Node {
    ///     value: u32,
    ///     tail: Option<Gc<Node>>,
    /// }
    ///
    /// impl Trace for Node {
    ///     // See the docs for `Trace` for details...
    /// #   fn trace(&self, _: &mut safe_gc::Collector) {}
    /// }
    ///
    /// let mut heap = Heap::new();
    ///
    /// let a = heap.alloc(Node { value: 0, tail: None });
    /// let b = heap.alloc(Node { value: 1, tail: Some(a.into()) });
    ///
    /// // Get a reference to a `Gc<T>` from the heap.
    /// let b_tail: Gc<Node> = heap[b].tail.unwrap();
    ///
    /// // Upgrade the `Gc<T>` to a `Root<T>` via the `Heap::root` method so it is
    /// // suitable for holding across garbage collections.
    /// let b_tail: Root<Node> = heap.root(b_tail);
    ///
    /// // Now we can perform operations, like heap allocation, that might GC
    /// // without worrying about `b_tail`'s referenced GC object from being
    /// // collected out from under us.
    /// heap.alloc(Node { value: 123, tail: None });
    ///
    /// // And we can access `b_tail`'s referenced GC object again, after GC may
    /// // have happened.
    /// let b_tail_value = heap[&b_tail].value;
    /// assert_eq!(b_tail_value, 0);
    /// ```
    #[inline]
    pub fn root<T>(&self, gc: Gc<T>) -> Root<T>
    where
        T: Trace,
    {
        assert_eq!(self.id, gc.heap_id);
        let arena = self.arena::<T>().unwrap();
        arena.root(gc)
    }

    /// Collect garbage.
    ///
    /// Any object in the heap that is not transitively referenced by a
    /// [`Root<T>`][crate::Root] will be reclaimed.
    ///
    /// # Example
    ///
    /// ```
    /// use safe_gc::Heap;
    ///
    /// let mut heap = Heap::new();
    ///
    /// # let allocate_a_bunch_of_stuff = |_| {};
    /// allocate_a_bunch_of_stuff(&mut heap);
    ///
    /// // Collect garbage!
    /// heap.gc();
    /// ```
    #[inline(never)]
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
