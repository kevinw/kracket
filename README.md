# unnamed-scheme-variant x86/arm compiler

## TODO

- general scheme criticisms:
  - top level function namespace is HUGE
- get an ARM test path working with qemu
- get an xcode project with code actually running on a physical iphone
- closures, etc. continue "an incremental approach to compiler construction"
- start sketching out the simplification of semantics that will make up the "schemelike"
- start outlining major language design goals for where it will diverge from the original (Steele 1975)
    - customaizable memory layout for natural and elegant data-driven programming
        - ARM/x86 vectorization, if not automatic in the compiler, should at least be natural to express as a side effect
    - per-frame allocators as easy to use as garbage collection (as easy to use as manual memory management?)
        - how do those things interact? research rust's different pointer types for inspiration
