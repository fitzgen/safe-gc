//! Simple bit set implementation for marking.

#[derive(Default)]
pub struct MarkBits(Vec<u8>);

impl MarkBits {
    /// Reset these mark bits, and ensure there are enough bits for the given
    /// capacity.
    pub fn reset(&mut self, capacity: usize) {
        self.0.clear();
        self.0.resize(capacity, 0);
    }

    /// Get the mark bit for the given index.
    ///
    /// Panics if the index is out of bounds.
    pub fn get(&self, index: u32) -> bool {
        let index = usize::try_from(index).unwrap();
        let byte_index = index / 8;
        let bit_index = index % 8;
        let mask = 1 << bit_index;
        self.0[byte_index] & mask != 0
    }

    /// Sets the mark bit for the given index, and returns the old mark bit
    /// state.
    ///
    /// Panics if the index is out of bounds.
    pub fn set(&mut self, index: u32) -> bool {
        let index = usize::try_from(index).unwrap();
        let byte_index = index / 8;
        let bit_index = index % 8;
        let mask = 1 << bit_index;
        let old_byte = self.0[byte_index];
        self.0[byte_index] = old_byte | mask;
        old_byte & mask != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mark_bits() {
        let mut mark_bits = MarkBits::default();

        for i in 0..32 {
            mark_bits.reset(32);

            for j in 0..32 {
                assert_eq!(mark_bits.get(j), false);
            }

            let was_already_set = mark_bits.set(i);
            assert!(!was_already_set);
            let was_already_set = mark_bits.set(i);
            assert!(was_already_set);

            if i > 0 {
                let was_already_set = mark_bits.set(i - 1);
                assert!(!was_already_set);
                let was_already_set = mark_bits.set(i - 1);
                assert!(was_already_set);
                assert!(mark_bits.get(i));
            }

            if i < 31 {
                let was_already_set = mark_bits.set(i + 1);
                assert!(!was_already_set);
                let was_already_set = mark_bits.set(i + 1);
                assert!(was_already_set);
                assert!(mark_bits.get(i));
            }
        }
    }
}
