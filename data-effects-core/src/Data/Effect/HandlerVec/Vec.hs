{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

{- |
Copyright: (c) 2022 Xy Ren
License: BSD3
Maintainer: xy.r@outlook.com
Stability: experimental
Portability: non-portable (GHC only)
-}
module Data.Effect.HandlerVec.Vec where

import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Primitive.Array (
    Array,
    cloneArray,
    copyArray,
    emptyArray,
    indexArray,
    mapArray',
    newArray,
    runArray,
    thawArray,
    writeArray,
 )
import Prelude hiding (concat, drop, head, length, map, tail, take)

-- TODO: remove len then bench

-- | A vector (i.e. array slice) type.
data Vec (a :: Type) = Vec !Int !Int !(Array a)

nil :: a
nil = error "Sp.Internal.Vec: uninitialized element"

-- | Get the length of the vector. \( O(1) \).
length :: Vec a -> Int
length (Vec _ len _) = len

-- | Create an empty record. \( O(1) \).
empty :: Vec a
empty = Vec 0 0 emptyArray

singleton :: a -> Vec a
singleton x = Vec 0 1 $ runArray $ newArray 1 x

-- | Prepend one entry to the vector. \( O(n) \).
cons :: a -> Vec a -> Vec a
cons x (Vec off len arr) = Vec 0 (len + 1) $ runArray do
    marr <- newArray (len + 1) x
    copyArray marr 1 arr off len
    pure marr

-- | Concatenate two vectors. \( O(m+n) \).
concat :: Vec a -> Vec a -> Vec a
concat (Vec off len arr) (Vec off' len' arr') = Vec 0 (len + len') $ runArray do
    marr <- newArray (len + len') nil
    copyArray marr 0 arr off len
    copyArray marr len arr' off' len'
    pure marr

-- | Get the head of the vector. \( O(1) \).
head :: Vec a -> a
head (Vec off _ arr) = indexArray arr off

-- | Slice off one entry from the head of the vector. \( O(1) \).
tail :: Vec a -> Vec a
tail (Vec off len arr) = Vec (off + 1) (len - 1) arr

-- | Slice off several entries from the head of the vector. \( O(1) \).
drop :: Int -> Vec a -> Vec a
drop len' (Vec off len arr) = Vec (off + len') (len - len') arr

-- | Take elements from the head of the vector. \( O(m) \).
take :: Int -> Vec a -> Vec a
take len (Vec off _ arr) = Vec off len arr

-- | Get an element in the vector. \( O(1) \).
index :: Int -> Vec a -> a
index ix (Vec off _ arr) = indexArray arr (off + ix)

-- | Update an entry in the record. \( O(n) \).
update :: Int -> a -> Vec a -> Vec a
update ix x (Vec off len arr) = Vec 0 len $ runArray do
    marr <- thawArray arr off len
    writeArray marr ix x
    pure marr

map :: (a -> b) -> Vec a -> Vec b
map f (Vec off len arr) = Vec off len (mapArray' f arr)

removesUnder :: Int -> Vec a -> Vec a
removesUnder n (Vec off len arr) =
    let len' = len - n
     in Vec 0 len' $ runArray do
            marr <- newArray len' (indexArray arr off)
            copyArray marr 1 arr (off + 1 + n) (len' - 1)
            pure marr

removesUnders :: Int -> Int -> Vec a -> Vec a
removesUnders start n (Vec off len arr) =
    let len' = len - n
     in Vec 0 len' $ runArray do
            marr <- thawArray arr off len'
            copyArray marr 1 arr (off + start + n) (len' - start)
            pure marr

-- | Get a known subset of the vector. \( O(m) \).
pick :: Int -> [Int] -> Vec a -> Vec a
pick len ixs (Vec off _ arr) = Vec 0 len $ runArray do
    marr <- newArray len nil
    for_ (zip [0 ..] ixs) \(newIx, oldIx) ->
        writeArray marr newIx $ indexArray arr (off + oldIx)
    pure marr

-- | A drop operation: either empty the entire vector or drop a certain number of entries from the head.
data DropPhase = EmptyOp | DropOp !Int

{- | A drop-and-concat operation: perform a drop operation, then optionally concat back some entries from the original
vector.
-}
data ConcatPhase = IdOp DropPhase | ConcatOp !Int [Int] DropPhase

-- | Extract a subset out of the vector. \( O(n) \).
extract :: ConcatPhase -> Vec a -> Vec a
extract ext (Vec off len arr) = case ext of
    IdOp ro -> case ro of
        EmptyOp -> Vec 0 0 emptyArray
        DropOp dropped -> Vec (off + dropped) (len - dropped) arr
    ConcatOp added addIxs ro -> case ro of
        EmptyOp -> Vec 0 added $ runArray do
            marr <- newArray added nil
            for_ (zip [0 ..] addIxs) \(newIx, oldIx) ->
                writeArray marr newIx $ indexArray arr (off + oldIx)
            pure marr
        DropOp dropped -> Vec 0 (len - dropped + added) $ runArray do
            marr <- newArray (len - dropped + added) nil
            for_ (zip [0 ..] addIxs) \(newIx, oldIx) ->
                writeArray marr newIx $ indexArray arr (off + oldIx)
            for_ [0 .. len - dropped - 1] \ix ->
                writeArray marr (added + ix) $ indexArray arr (off + dropped + ix)
            pure marr
