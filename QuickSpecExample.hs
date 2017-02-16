{-
  stack
  --resolver lts-8.0
  --install-ghc
  exec
  --package base
  --package quickspec
  --package QuickCheck
  --package testing-feat
  --
  ghc -O2
-}

-- Copyright 2017 Google Inc. All Rights Reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


{-# LANGUAGE ScopedTypeVariables, GADTs, KindSignatures, StandaloneDeriving, TemplateHaskell, DeriveGeneric #-}

import Prelude hiding (sum)
import Control.Applicative (liftA2)
import Test.QuickSpec
import Test.QuickCheck
import Data.Typeable
import Test.Feat (deriveEnumerable, uniform)
import Test.Feat.Class
import Data.Monoid ((<>))
import GHC.Generics

data BTree = Leaf | Branch BTree BTree
  deriving (Eq, Ord, Show, Generic)

data Finite'
  = Empty
  | Singleton BTree
  | Sum Finite Finite
  | Prod Finite Finite
  | FMap (BTree -> BTree) Finite

data Finite = Finite { card :: Integer, fin :: Finite' }

empty = Finite 0 Empty
singleton x = Finite 1 (Singleton x)
sum f1 f2 = Finite (card f1 + card f2) (Sum f1 f2)
prod f1 f2 = Finite (card f1 * card f2) (Prod f1 f2)
fMap f fi = Finite (card fi) (FMap f fi)

index :: Finite -> Integer -> Maybe BTree
index (Finite c f) i
  | i >= c = Nothing
  | otherwise = index' f
  where index' (Singleton x) = Just x
        index' (Sum f1 f2) | i < card f1 = index f1 i
                           | otherwise = index f2 (i - card f1)
        index' (Prod f1 f2) = liftA2 Branch (index f1 q) (index f2 r)
          where (q, r) = divMod i (card f2)
        index' (FMap f fi) = fmap f (index fi i)

toList :: Finite -> [Maybe BTree]
toList f = [index f i | i <- [0..card f]]

instance Eq Finite where
  f1 == f2 = toList f1 == toList f2

instance Ord Finite where
  compare f1 f2 = compare (toList f1) (toList f2)

instance Enumerable Finite where
  enumerate =
    consts [ pure empty
           , unary singleton
           , unary (funcurry sum)
           , unary (funcurry prod)
           ]

instance Arbitrary Finite where
  arbitrary = sized uniform

deriveEnumerable ''BTree

instance Arbitrary BTree where
  arbitrary = sized uniform

instance CoArbitrary BTree

feat :: [Sig]
feat =
  [ ["x","y","z"] `vars` (undefined :: Finite)
  , funs (undefined :: BTree)
  , "empty" `fun0` empty
  , "sum" `fun2` sum
  , "prod" `fun2` prod
  , "fmap" `fun2` fMap
  ]

main = quickSpec feat

