# data-effects

[![Hackage](https://img.shields.io/hackage/v/data-effects.svg?logo=haskell&label=data-effects)](https://hackage.haskell.org/package/data-effects)
[![Hackage](https://img.shields.io/hackage/v/data-effects-core.svg?logo=haskell&label=data-effects-core)](https://hackage.haskell.org/package/data-effects-core)
[![Hackage](https://img.shields.io/hackage/v/data-effects-th.svg?logo=haskell&label=data-effects-th)](https://hackage.haskell.org/package/data-effects-th)

A basic framework for a Haskell effect system library based on GADTs-based effect representations
with a style that separates first-order effects and higher-order effects.

This library set was created by being separated from
the [Heftia](https://github.com/sayo-hs/heftia) extensible effects library.

## Your contributions are welcome!
Please see [CONTRIBUTING.md](https://github.com/sayo-hs/data-effects/blob/ef706ef3fa547de01ce6bb5636af911354e53b58/CONTRIBUTING.md).

## Citations
The following is a list of existing works that have been modified and used within this library:

* **[compdata](https://github.com/pa-ba/compdata)**
    * **Copyright** (c) 2010--2011 Patrick Bahr, Tom Hvitved
    * **License**: BSD-3-Clause
    * **Modifications**: Used TemplateHaskell code to derive instances of the `HFunctor` type class.
