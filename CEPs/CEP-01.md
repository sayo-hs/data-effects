# CEP-01 - Effect Class

CEP-01 is the foundation for all CEPs.

## Definitions
* "Defined Together"

    When the expression "X and Y are defined together" is used in CEPs, it implies that the defined entity X SHOULD be in the same Haskell module where Y is also defined. If for some reason it's considered better not to, the documentation related to the defined entity X MUST provide a link or some reference to where Y can be found.

## Unified Representation of Effect Class
An effect class MUST be represented by a type class in the following format:

* The type class representing the effect class is defined as an *effect type-class*.
* Each method in the type class is defined as an *effect method*, or simply as an *effect*.
    * The type of the effect method is defined as the *type of effect*.
* The kind of the type class is:

    ```haskell
    <K_1> -> <K_2> -> ... -> <K_n> -> (Type -> Type) -> Constraint
    ```

    * `forall` can be inserted at any position.
        * For example, `forall k1. k1 -> forall k2. (k2 -> k1) -> (Type -> Type) -> Constraint` is valid.

    * The type arguments corresponding to the kinds from `<K_1>` to `<K_n>` are defined as *effect type parameters*.

    * The monad-like type in the `(Type -> Type)` section is defined as a *carrier*, often represented by the letter `f`.
        * Note: This section typically contains the type of the monad that handles the effect, but it is generalized for non-monads like Applicatives (Applicative effects).

    * For example, in the effect type-class for the state effect:

        ```haskell
        class State (s :: Type) (f :: Type -> Type) where
            get :: f s
            put :: s -> f ()
        ```

        The effect type parameter is `s` and the carrier is `f`.

## Separation of First-order and Higher-order Effects
An effect type class MUST consist only of either first-order effect methods or higher-order effect methods.

* You can't mix both first-order and higher-order methods in a single effect type class.
* The type of the method MUST take the form `<A_1> -> <A_2> -> ... -> <A_n> -> f <B>`.
    * `forall` can be inserted at any position.
        * For example, `forall x. x -> forall y. (y -> z) -> f x` is valid.

* *First-order effects* are defined as those where the carrier type variable `f` doesn't appear in all from `<A_1>` to `<A_n>`.
* *Higher-order effects* are defined as any effects that aren't first-order, meaning the carrier `f` appears in their types.

* Effect classes that only contain first-order effects are defined as *first-order effect types*, and those containing only higher-order effects are defined as *higher-order effect types*.

## Inclusion of Effect Classes
If you want to express a situation where one effect class includes the effects of another, you SHOULD use superclasses in the definition of the effect type class.

* Example: Reader effect class

    The Reader effect class contains the first-order effect `ask` and the higher-order effect `local`. If you try to express this naively, it would violate the rule that prevents mixing first-order and higher-order effects in a single effect type class. Hence, express the Reader effect class by declaring a type class without any methods, as follows:

    ```haskell
    class (Ask r f, Local r f) => Reader r f

    class Ask (r :: Type) f where
        ask :: f r

    class Local r f where
        local :: (r -> r) -> f a -> f a
    ```

## Elimination of Duplicate Effect Definitions and Laws of Effects
The definition of equivalent effect classes MUST not be duplicated more than once.

* However, this equivalence is **not** merely that all the types of methods in the effect type classes are the same, but also whether the *laws* of the effects are the same.

* For example, between `Ask` and `Input`:

    The `Input` effect class is defined as:

    ```haskell
    class Input (i :: Type) f where
        input :: f i
    ```

    The types of effects in the `Input` and `Ask` effect classes are equivalent. However, when the `Ask` effect class is used with the `Local` effect class, there's a interaction law:

    ```haskell
    ∀(f :: r -> r). local f ask = f <$> ask
    ```

    This law doesn't exist in the `Input` effect class. In general:

    ```haskell
    local f input ≠ f <$> input
    ```

    It is recommended to explicitly document the laws of effects, but it is not mandatory. Customary laws may exist. Conversely, if all the effect types of the effect class are the same, but the effect classes are not equivalent, it implies that the laws of the effects are different.

    * Note: The fact that customary laws can exist means that the equivalence determination of effects and effect classes is not mechanically or formally possible.

    Handlers (carriers) SHOULD ensure they handle effects while adhering to the laws of effects.

## Expression of Carrier for Each Backend
Each effect system backend SHOULD define a unique identifier expressed with a type tag, and by using this type tag as the first type argument for the `EffectsVia` data type in the `classy-effects-base` package, SHOULD implement the instance of the effect for that carrier. This approach realizes a backend-specific effect handling mechanism.

Example: CEP-02 itself serves as an example of one backend. Refer to the section on "Sending Effect Data to the Carrier" in CEP-02. Here, the type tag `EffectDataHandler` is the unique identifier for the CEP-02 backend.

## Recommendation on Interface Independence from the Effect System Backend
If possible (if there's no need to depend on the elements contained in the mechanism of the effect system backend library), the interface of the effectful functions SHOULD consist only of definitions compliant with CEP.

* Specifically, constraints of the effect are given by the effect type class, and the monad is polymorphized and generalized with the carrier type variable.
* Example: In the (fictional) effect system library `foobar-effects`, with the type family `Member` provided by this library to represent the constraints of the effect and the `Eff` monad of extensible effects, you can write effectful functions like:

    ```haskell
    updateCurrentTemperature
        ::  FoobarEffects.Member (FoobarEffects.StateI Temperature) r =>
            Double -> FoobarEffects.Eff r ()
    updateCurrentTemperature temp = FoobarEffects.put (temperatureFromDouble temp)
    ```

    (Here, `FoobarEffects.StateI` is a State effect class represented using GADTs. Please refer to CEP-02 for more details.)

    However, since it is just issuing an effect here, instead of doing this, using the `State` effect type class defined in accordance with CEP:

    ```haskell
    updateCurrentTemperature :: CepStandard.State Temperature f => Double -> f ()
    updateCurrentTemperature temp = CepStandard.put (temperatureFromDouble temp)
    ```

    It is possible to write functions that depend only on the definitions by CEP. And in general, effectful functions SHOULD be written this way.

    * Note: This is because, by polymorphizing the carrier type `f` and expressing the constraints with the CEP-compliant effect type class, it becomes possible to handle effectful functions in various effect system backend implementations without being locked into a specific effect system library implementation.
        * Also, the possibility of generalizing not only to monads but also to applicative effects arises.

## Naming Rules
For effects where first-order effects and higher-order effects were originally mixed to form a single effect class, when separating, naming SHOULD be done as follows:

1. For those where one first-order effect and one higher-order effect combined to form a single effect class, the name of the effect for each order remains as the effect class name.

    * Example: `Reader` has `ask` and `local` as first and higher order effects respectively, so they should be separated into the `Ask` effect class and the `Local` effect class.

2. For those where not just one of each order but multiple effects exist on one order:

    1. If multiple effects exist on the first-order side, the original effect name should have `F` added at the end to form the effect class name.

    2. If multiple effects exist on the higher-order side, the original effect name should have `H` added at the end to form the effect class name.

        * Example: `Writer` has one `tell` effect on the first-order side and two effects, `listen` and `censor`, on the higher-order side (referencing the definition of [`Writer`](https://hackage.haskell.org/package/fused-effects-1.1.2.2/docs/Control-Effect-Writer.html) in [fused-effects](https://hackage.haskell.org/package/fused-effects)), so it should be as follows:

            ```haskell
            class Tell w f where
                tell :: w -> f ()

            class WriterH w f where
                listen :: f a -> f (w, a)
                censor :: (w -> w) -> f a -> f a

            class (Tell w f, WriterH w f) => Writer w f
            ```

    * Note: `F` stands for 'First-order' and `H` stands for 'Higher-order'.

### Module Namespaces
Modules that define effect type classes SHOULD be placed under `Control.Effect.Class.`.

Note: It's good to consider effect classes with interaction laws (e.g., `Ask`, `Local`, and `Reader`) as a single unit and group them into one module. For example, `Control.Effect.Class.State`, `Control.Effect.Class.Writer`, `Control.Effect.Class.Reader`.
