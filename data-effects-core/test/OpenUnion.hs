{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- SPDX-License-Identifier: MPL-2.0

module OpenUnion where

import Control.Effect (Eff, Free, perform)
import Data.Effect
import Data.Effect.OpenUnion (Membership (UnsafeMembership), labelMembership, (:>))
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat, Natural, natVal)
import Test.Hspec (Spec, describe, it, shouldBe)

data A x :: Effect
data ALabel
type instance LabelOf (A x) = ALabel

data B x y :: Effect
data BLabel x
type instance LabelOf (B x y) = BLabel x

spec_membership :: Spec
spec_membership = describe "Open-Union Membership" do
    it "Membership (A 0) '[B 1 2, A 0] -> 1" $
        label1 `shouldBe` UnsafeMembership 1

    it "Membership (B 5 6) '[B 1 2, B 3 4, A 1, B 5 6, A 0] -> 3" $
        label2 `shouldBe` UnsafeMembership 3

    it "Membership (B 5 ?) '[B 1 2, B 3 4, A 1, B 5 6, A 0] --infer--> ?=6" $
        infer1 labelMembership `shouldBe` 6
  where
    label1 :: Membership (A 0) '[B 1 2, A 0]
    label1 = labelMembership

    label2 :: Membership (B 5 6) '[B 1 2, B 3 4, A 1, B 5 6, A 0]
    label2 = labelMembership

    infer1 :: forall x. (KnownNat x) => Membership (B 5 x) '[B 1 2, B 3 4, A 1, B 5 6, A 0] -> Natural
    infer1 _ = natVal @x Proxy

inferCompileTest :: (Throw () :> es, Catch () :> es, Free Monad ff) => Eff ff es a
inferCompileTest = perform $ Throw mempty
