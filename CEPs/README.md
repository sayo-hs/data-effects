# CEPs

CEPs (Classy-Effects Protocols) are a collection of protocols for expressing and defining effects in Haskell, aimed at the first objective in the [Sayo Project](https://github.com/sayo-hs):

> Exploring ways to improve interoperability among the myriad Haskell effect system libraries

CEP-01 is at the core of these protocols. Each CEP is independent, and some are extensions (optional).

The current version is 0.1.0 (compliant with SemVer). The current state is experimental and not stable.

## Libraries
The `classy-effects-base` library provides the definitions necessary to comply with CEPs.

The `classy-effects-th` library offers a feature to derive all the definitions required for CEPs compliance solely from the type class definitions of effects, using Template Haskell. By using this automatic derivation, you essentially comply with CEPs without needing to be consciously aware of them.

The `classy-effects` library provides standard effects (`Reader`, `Writer`, `State`, etc.) defined in accordance with CEPs.

## License
All files located under the CEPs directory where this README.md file is placed are licensed under CC BY-SA 4.0.
