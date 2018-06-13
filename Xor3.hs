#!/usr/bin/env stack
{-
  stack
  --resolver lts-11.12
  --install-ghc
  runghc
  --package lazysmallcheck-0.6
  --package testing-feat
  --
  -hide-all-packages
-}

-- Copyright 2016 Google Inc. All Rights Reserved.
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

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.LazySmallCheck
import Test.Feat

data Expr = A | B | C | Not Expr | And Expr Expr | Or Expr Expr | Xor Expr Expr
  deriving Show

instance Serial Expr where
  series = cons0 A \/ cons0 B \/ cons0 C \/ cons1 Not \/ cons2 And \/ cons2 Or \/ cons2 Xor

deriveEnumerable ''Expr

eval lift and or eq not a b c = eval'
  where eval' A = lift a
        eval' B = lift b
        eval' C = lift c
        eval' (Not e) = not (eval' e)
        eval' (And e1 e2) = eval' e1 `and` eval' e2
        eval' (Or e1 e2) = eval' e1 `or` eval' e2
        eval' (Xor e1 e2) = eval' e1 `eq` not (eval' e2)

isNotXor3 lift and or eq not e =
       not (eval' True False False e)
  `or` not (eval' False True False e)
  `or` not (eval' False False True e)
  `or` eval' False False False e
  `or` eval' True True False e
  `or` eval' True False True e
  `or` eval' False True True e
  `or` eval' True True True e
    where
      eval' = eval lift and or eq not

isNotXor3Prop = isNotXor3 lift (*&*) (*|*) (*=*) neg
isNotXor3Pred = isNotXor3 id (&&) (||) (==) not

main = do
  featCheck 10 isNotXor3Pred
  smallCheck 10 isNotXor3Prop
