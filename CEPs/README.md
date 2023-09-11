# CEPs

CEPs (Classy-Effects Protocols) is a collection of protocols for expressing and defining effects in Haskell, with the second goal of the sayo-project:

* To explore ways to improve interoperability among the disparate Haskell effect system libraries.

CEP-01 is the core of the protocol.
Each CEP is independent, and some are extensions (optional).

The current version is 0.1.0 (SemVer compliant).
The current status is experimental and not stable.

## Libraries
The `classy-effects-base` library provides the definitions needed for CEPs compliance.

The `classy-effects-th` library provides functionality for Template Haskell to derive all the definitions needed for CEPs compliance from just the type-class definitions of the effects.
Using this automatic derivation, you are automatically compliant with CEPs and basically do not need to be aware of CEPs.

The `classy-effects` library provides standard effects (`Reader`, `Writer`, `State`, ...) defined in a CEPs-compliant manner.

## License
All files located under the CEPs directory where this README.md file is placed are licensed under CC BY-SA 4.0.
