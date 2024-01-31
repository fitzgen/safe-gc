#![no_main]

use libfuzzer_sys::fuzz_target;
use safe_gc_mutator::Mutator;

fuzz_target!(|mutator: Mutator| {
    mutator.run();
});
