<div align="center">
  <h1><code>safe-gc</code></h1>
  <p><strong>A garbage collection library for Rust without any <code>unsafe</code> code</strong></p>
  <p>
    <a href="https://github.com/fitzgen/safe-gc/actions/workflows/rust.yml"><img src="https://github.com/fitzgen/safe-gc/actions/workflows/rust.yml/badge.svg" alt="build status" /></a>
    <a href="https://docs.rs/safe-gc"><img src="https://docs.rs/safe-gc/badge.svg" alt="Documentation Status" /></a>
  </p>
</div>

## About

`safe-gc` implements a garbage collection library for Rust with zero `unsafe`
code and zero dependencies. It even has a `forbid(unsafe_code)` directive at the
top!

Additional features:

* Allows constructing and collecting arbitrary heap graphs, including cycles. It
  doesn't impose any ownership hierarchy, or anything like that, to the shapes
  of references between GC-managed objects within the heap.

* Leverages Rust's ownership and borrowing in its API: if you have an `&mut
  Heap`, you can get mutable access to objects in the heap. It doesn't, for
  example, force everything in the heap into `RefCell`s, or only give out shared
  references to GC-managed objects, or similar.

* Allows constructing multiple, separate GC heaps that can be independently
  collected.

* Allows allocating any number of heterogeneous types within the heap. For
  example, you can allocate both `Cons` and `Tree` objects within the
  heap. Heaps are *not* constrained to only a single, uniform `T` type of GC
  objects.

`safe-gc` is not, however, a particularly high-performance garbage collector.

## Usage

* Define types managed by the GC.

* Define references from within one GC type to another GC type with `Gc<T>`.

* Implement `Trace` for your GC-managed types.

* Create one or more `Heap`s.

* Allocate objects in your `Heap`s.

* Hold onto GC roots with `Root<T>`.

* Let the garbage collector reclaim unreachable objects!

## Example

```rust
use safe_gc::{Collector, Gc, Heap, Trace};

// Define a GC-managed tree of `T` values.
struct Tree<T: Trace> {
    value: Gc<T>,

    // A cyclic parent pointer.
    parent: Option<Gc<Tree<T>>>,

    // Left and right subtrees.
    left: Option<Gc<Tree<T>>>,
    right: Option<Gc<Tree<T>>>,
}

// Report each of the GC references within a `Tree` to the
// collector.
//
// See the `Trace` docs for more details.
impl<T: Trace> Trace for Tree<T> {
    fn trace(&self, collector: &mut Collector) {
        collector.edge(self.value);
        if let Some(parent) = self.parent {
            collector.edge(parent);
        }
        if let Some(left) = self.left {
            collector.edge(left);
        }
        if let Some(right) = self.right {
            collector.edge(right);
        }
    }
}

// Another GC type!
struct Cat {
    cuteness: u32,
    cat_tree: Option<Gc<Tree<Cat>>>,
}

impl Trace for Cat {
    fn trace(&self, collector: &mut Collector) {
        if let Some(tree) = self.cat_tree {
            collector.edge(tree);
        }
    }
}

// Create a new GC heap!
let mut heap = Heap::new();

// Allocate some objects in the heap!
let momo = heap.alloc(Cat {
    cuteness: u32::MAX,
    cat_tree: None,
});
let tree = heap.alloc(Tree {
    value: momo.unrooted(),
    parent: None,
    left: None,
    right: None,
});

// Create a bunch of garbage! Who cares!
for _ in 0..100 {
    let _ = heap.alloc(Tree {
        value: momo.unrooted(),
        parent: None,
        left: None,
        right: None,
    });
}

// Read data from objects in the heap!
let cuteness = heap[&momo].cuteness;
assert_eq!(cuteness, u32::MAX);

// Mutate objects in the heap!
heap[&momo].cat_tree = Some(tree.into());

// Garbage collections will happen automatically, as necessary, but you can also
// force a collection, if you want!
heap.gc();
```

## Why?

`safe-gc` is certainly a point in the design space of garbage-collection
libraries in Rust. One could even argue it is an interesting -- and maybe even
useful? -- point in the design space!

Also, it was fun!

At the very least, you don't have to wonder about the correctness of any
`unsafe` code in here, because there isn't any. As long as the Rust language and
its standard library are sound, this crate is too.
