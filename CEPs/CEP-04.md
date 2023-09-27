# CEP-04 - Synonyms for Order Lifting of Instruction (optional)

CEP-04 provides type synonyms and pattern synonyms to omit `LiftIns` offered by the `classy-effects-base` package.

Required extension: CEP-02

## Type Synonyms
All first-order effect classes that conform to CEP-04 MUST define together a type synonym of the following form for their instruction class `<TI>`:

```haskell
type <TS> <P_1> <P_2> ... <P_n> = LiftIns (<TI> <P_1> <P_2> ... <P_n>)
```

Here, if `<TI>` follows the naming convention in CEP-02 where it ends with `I`, it MUST replace `I` with `S` to get the type synonym name `<TS>`. If not, the name `<TS>` MUST be the name of `<TI>` with an `S` added to the end.

## Pattern Synonyms
All first-order effect classes that conform to CEP-04 MUST define together a pattern synonym of the following form for their instruction `<Ins>`:

```haskell
pattern <InsFS> x1 x2 ... xn = LiftIns (<Ins> x1 x2 ... xn)
```

Here, `<InsFS>` MUST be the name of `<Ins>` with an `FS` added to the end.
