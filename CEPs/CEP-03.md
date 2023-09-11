# CEP-03 - Heftia backend support (optional)

CEP-03 is designed to support the Heftia effect system backend.

Required Extension: CEP-02

## About HFunctor

1. Every higher-order effect class data-type MUST implement the `HFunctor` type class provided in the `compdata` package as an instance.

2. Every higher-order effect class data-type SHOULD implement the `HFunctor` provided by the `classy-effects-base` package, which re-exports the `HFunctor` from the `compdata` package, as an instance.

    * Note: This is considering the possibility of discontinuing the re-export of `compdata`'s `HFunctor` in the future. Currently, there is no such plan, but this SHOULD recommendation is in place as a precaution in case any issues are found with this approach. As of now, by adopting the method of re-exporting the `HFunctor` provided by the `compdata` package, the existing `HFunctor` instances are not wasted, hence the provision in this form of re-export.
