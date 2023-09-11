# CEP-02 - Effect Data (optional)

CEP-02 defines rules to uniquely specify the data type representation of effect classes from type classes representing effect classes. It enable the provision of a mechanism for each effect system backend that operates based on effect data to delegate handling to the derived data type representation.

## Data Type Representation of Effect Class
All effect classes conforming to CEP-02 MUST define together the type class and this data type representation in the code. However, for effect type classes with empty methods (e.g., `Reader`, `Writer` effect classes), this requirement does not apply (refer to the "Inclusion of Effect Classes" section in CEP-01).

* The data type representing the effect class, uniquely derived from effect type classes, is referred to as the *effect class data-type*, and each constructor of the data type is called *effect data*.

* The kind of the effect class data-type MUST take the form:

    ```haskell
    <K_1> -> <K_2> -> ... -> <K_n> -> Type -> Type
    ```

    for first-order effect classes, or

    ```haskell
    <K_1> -> <K_2> -> ... -> <K_n> -> (Type -> Type) -> Type -> Type
    ```

    for higher-order effect classes.

* First-order effect class data-types and effect data are defined as *instruction classes* and *instructions*, respectively, while higher-order ones are defined as *signature classes* and *signatures*.

### Correspondence Rules Between Effect Methods and Effect Data
The methods and constructors (effect data) of an effect class, present in both type classes and data types, MUST correspond one-to-one according to the following rule:

* The type of the constructor (effect data) must correspond to the type of the effect method:

    ```haskell
    <A_1> -> <A_2> -> ... -> <A_n> -> f <B>
    ```

    as follows:

    - For first-order effect classes:

    ```haskell
    forall <FVs>. <A_1> -> <A_2> -> ... -> <A_n> -> <T> <P_1> <P_2> ... <P_n> <B>
    ```

    - For higher-order effect classes:

    ```haskell
    forall <FVs>. <A_1> -> <A_2> -> ... -> <A_n> -> <T> <P_1> <P_2> ... <P_n> f <B>
    ```

    where `<T>` is the effect class data-type, `<P_1>` to `<P_n>` are effect type parameters, and `f` is the carrier type variable. Also, `<FVs>` lists all the free variables that appear later in the order they appear. However, effect type parameters are prioritized and placed at the beginning, before other free variables.
    * Note: This ordering is important when using the `TypeApplications` extension.

    The `forall` inserted between the effect method types (see the "Separation of First-order and Higher-order Effects" section in CEP-01) is simply ignored.

* Examples:
    1. When the name of the effect class data-type of State is `StateI`, the State effect class:

        ```haskell
        class State s f where
            get :: f s
            put :: s -> f ()
        ```

        corresponds to the following effect class data-type since the State effect class is first-order:

        ```haskell
        data StateI s a where
            Get :: StateI s s
            Put :: s -> StateI s ()
        ```

    1. For the Local effect class, which is a subclass of the Reader effect class, when the name of the effect class data-type is `LocalS`, the Local effect class:

        ```haskell
        class Local r f where
            local :: (r -> r) -> f a -> f a
        ```

        corresponds to the following effect class data type, considering that the Local effect class is higher-order:

        ```haskell
        data LocalS r f a where
            Local :: (r -> r) -> f a -> LocalS r f a
        ```

        Alternatively, it can also be represented in a non-GADT form as:

        ```haskell
        data LocalS r f a = Local (r -> r) (f a)
        ```

## Sending Effect Data to the Carrier
All effect class data-types MUST have an instance in the form where the data type `(EffectsVia EffectDataHandler f)` is substituted for the carrier type variable. In this instance, the implementation of each method MUST simply delegate the implementation to the `SendIns` type class for first-order effect class data-types and the `SendSig` type class for higher-order effect class data-types, respectively. Also, the constraints of the instance MUST consist only of these `SendIns`, `SendSig`, and constraints on effect type parameters in the effect type class. `EffectsVia`, `EffectDataHandler`, `SendIns`, and `SendSig` are provided from the `classy-effects-base` package. Refer to the `classy-effects-base` documentation.

* First-oder example:

    ```haskell
    class State s f where
        get :: f s
        put :: s -> f ()

    data StateI s a where
        Get :: StateI s s
        Put :: s -> StateI s ()

    instance SendIns (StateI s) f => State s (EffectVia EffectDataHandler f) where
        get = EffectsVia $ sendIns Get
        put s = EffectsVia $ sendIns (Put s)
    ```

    ```haskell
    class Monoid w => Tell w f where
        tell :: w -> f ()

    data TellI w a where
        Tell :: w -> TellI w ()

    instance (SendIns (TellI w) f, Monoid w) => Tell s (EffectVia EffectDataHandler f) where
        tell w = EffectsVia $ sendIns (Tell w)
    ```

* Higher-order example:

    ```haskell
    class Catch e (f :: Type -> Type) where
        catch :: f a -> (e -> f a) -> f a

    data CatchS e f (a :: Type) where
        Catch :: f a -> (e -> f a) -> CatchS s f a

    instance HFunctor (CatchS e) where
        hfmap f (Catch a k) = Catch (hfmap f a) (hfmap f . k)

    instance SendSig (CatchS e) f => State s (EffectVia EffectDataHandler f) where
        catch a k = EffectsVia $ hfmap runEffectVia $ sendSig (Catch a k)
    ```

* Note: `EffectDataHandler` is a type tag representing a backend based on the mechanism of CEP-02.
* Note: For `HFunctor`, refer to CEP-03. Each CEP is independent, and note that implementing `HFunctor` is not mandatory for compliance with CEP-02.

## Elimination of Duplicate Definitions for Effect Data Types

For a single effect class, defining more than two corresponding effect data types that overlap SHOULD be avoided.

* Note: However, for instance, even if there are data types that are isomorphic to each other, such as the `AskI` data type for the `Ask` class and the `InputI` data type for the `Input` class, if the originating effect classes are different, both can and should be defined. Refer to the "Elimination of Duplicate Effect Definitions and Laws of Effects" section in CEP-01.

* Note: The reason this rule is a SHOULD and not a MUST is that even if they are defined redundantly, they would be isomorphic data types. Thus, interoperability can later be restored by defining conversion functions between them. Although it's somewhat cumbersome, it doesn't turn into a fatal issue that can't be recovered later. On the other hand, if effect type classes are defined redundantly, introducing complex type-level mechanisms that delegate the implementation of one to the other is required to restore interoperability. Such a situation should be strongly avoided, hence the rule about eliminating duplicates in CEP-01 is a MUST.

## Naming Conventions

The names of instruction classes and signature classes SHOULD append `I` or `S`, respectively, to the end of the effect class names.

Effect data names SHOULD use the name of the effect as is, converting the first letter to uppercase.

If the effect class name follows the naming convention in CEP-01, where `F` or `H` is appended, it SHOULD be replaced with `I` or `S`, respectively, for the name of the effect class data-type.

* Example:
    ```haskell
    class Monoid w => Tell w f where
        tell :: w -> f ()

    class Monoid w => WriterH w f where
        listen :: f a -> f (w, a)
        censor :: (w -> w) -> f a -> f a

    class (Tell w f, WriterH w f) => Writer w f
    ```

    should be named as:

    ```haskell
    data TellI w a where
        Tell :: w -> TellI w a

    data WriterS w f a where
        Listen :: f a -> WriterS w f (w, a)
        Censor :: (w -> w) -> f a -> WriterS w f a
    ```

* Note: `I` stands for 'Instruction', and `S` stands for 'Signature'.
