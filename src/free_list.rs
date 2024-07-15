enum Entry<T> {
    /// An occupied entry holding a `T`.
    Occupied(T),

    /// A free entry that is also part of a linked list pointing to the next
    /// free entry, if any.
    Free(Option<u32>),
}

pub struct FreeList<T> {
    entries: Vec<Entry<T>>,

    /// The index of the first free entry in the free list.
    free: Option<u32>,

    len: usize,
}

impl<T> Default for FreeList<T> {
    fn default() -> Self {
        Self {
            entries: Vec::default(),
            free: None,
            len: 0,
        }
    }
}

impl<T> FreeList<T> {
    pub fn new() -> Self {
        FreeList::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let mut list = FreeList::new();
        list.reserve(capacity);
        list
    }

    pub fn reserve(&mut self, additional: usize) {
        self.entries.reserve(additional);

        // Note that we didn't call `reserve_exact`, so the vector and/or
        // allocator may have given us a little extra capacity if it was
        // convenient for them. So make sure to use the actual new capacity,
        // rather than the requested new capacity.
        while self.entries.len() < self.entries.capacity() {
            let index = u32::try_from(self.entries.len()).unwrap();
            let next_free = std::mem::replace(&mut self.free, Some(index));
            self.entries.push(Entry::Free(next_free));
        }
    }

    pub fn double_capacity(&mut self) {
        // Double our capacity to amortize the cost of resizing. But make sure
        // we add some amount of additional capacity, since doubling zero
        // capacity isn't useful.
        const MIN_CAPACITY: usize = 16;
        let additional = std::cmp::max(self.entries.capacity(), MIN_CAPACITY);
        self.reserve(additional);
    }

    pub fn capacity(&self) -> usize {
        self.entries.capacity()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn try_alloc(&mut self, value: T) -> Result<u32, T> {
        if let Some(index) = self.free {
            let next_free = match self.entries[usize::try_from(index).unwrap()] {
                Entry::Free(next_free) => next_free,
                Entry::Occupied { .. } => unreachable!(),
            };
            self.free = next_free;
            self.entries[usize::try_from(index).unwrap()] = Entry::Occupied(value);
            self.len += 1;
            Ok(index)
        } else {
            Err(value)
        }
    }

    pub fn alloc(&mut self, value: T) -> u32 {
        self.try_alloc(value).unwrap_or_else(|value| {
            // Reserve additional capacity, since we didn't have space for the
            // allocation.
            self.double_capacity();
            // After which the allocation will succeed.
            self.try_alloc(value).ok().unwrap()
        })
    }

    pub fn get(&self, index: u32) -> &T {
        match &self.entries[usize::try_from(index).unwrap()] {
            Entry::Occupied(x) => x,
            Entry::Free(_) => unreachable!(),
        }
    }

    pub fn get_mut(&mut self, index: u32) -> &mut T {
        match &mut self.entries[usize::try_from(index).unwrap()] {
            Entry::Occupied(x) => x,
            Entry::Free(_) => unreachable!(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (u32, &T)> + '_ {
        self.entries
            .iter()
            .enumerate()
            .filter_map(|(i, e)| match e {
                Entry::Occupied(x) => Some((u32::try_from(i).unwrap(), x)),
                Entry::Free(_) => None,
            })
    }

    pub fn dealloc(&mut self, index: u32) {
        match &mut self.entries[usize::try_from(index).unwrap()] {
            Entry::Free(_) => {}
            e @ Entry::Occupied(_) => {
                let next_free = std::mem::replace(&mut self.free, Some(index));
                *e = Entry::Free(next_free);
                self.len -= 1;
            }
        }
    }
}
