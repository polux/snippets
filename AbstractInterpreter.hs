#!/usr/bin/env stack
{-
  stack
  --resolver lts-11.10
  --install-ghc
  runghc
  --package base
  --package containers
  --
  -hide-all-packages
-}

-- Copyright 2018 Google LLC. All Rights Reserved.
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

module Main where

import qualified Data.Map.Strict as M

{- Syntax -}

data Expr
  = Var String
  | IntLit Int
  | BoolLit Bool
  | Plus Expr Expr
  | Lt Expr Expr
  deriving (Show)

data Instr
  = Assign String Expr
  | If Expr Instr Instr
  | While Expr Instr
  | Seq Instr Instr
  | Noop
  deriving (Show)

{- Concrete interpreter -}

data Value
  = VBool Bool
  | VInt Int
  deriving (Show)

type Store = M.Map String Value

emptyStore = M.empty

evalExpr :: Expr -> Store -> Value
evalExpr (Var x) s = s M.! x
evalExpr (IntLit i) _ = VInt i
evalExpr (BoolLit b) _ = VBool b
evalExpr (Plus e1 e2) s = VInt (i1 + i2)
  where VInt i1 = evalExpr e1 s
        VInt i2 = evalExpr e2 s
evalExpr (Lt e1 e2) s = VBool (i1 < i2)
  where VInt i1 = evalExpr e1 s
        VInt i2 = evalExpr e2 s

evalInstr :: Instr -> Store -> Store
evalInstr (Assign x e) s = M.insert x (evalExpr e s) s
evalInstr (If e i1 i2) s = if ve then evalInstr i1 s else evalInstr i2 s
  where VBool ve = evalExpr e s
evalInstr (While e i) s = if ve then evalInstr (While e i) (evalInstr i s) else s
  where VBool ve = evalExpr e s
evalInstr (Seq i1 i2) s = evalInstr i2 (evalInstr i1 s)
evalInstr Noop s = s

{- Abstract interpreter -}

data ABool = AFalse | ATrue | AnyBool
  deriving (Eq, Show)

data AInt = Zero | Neg | Pos | AnyInt
  deriving (Eq, Show)

data AValue
  = AVBool ABool
  | AVInt AInt
  deriving (Eq, Show)

type AStore = M.Map String AValue

emptyAStore = M.empty

aEvalExpr :: Expr -> AStore -> AValue
aEvalExpr (Var x) s = s M.! x
aEvalExpr (IntLit i) _ = AVInt ai
  where ai | i == 0 = Zero
           | i < 0  = Neg
           | i > 0  = Pos
aEvalExpr (BoolLit b) _ = AVBool (if b then ATrue else AFalse)
aEvalExpr (Plus e1 e2) s = AVInt (i1 `aplus` i2)
  where AVInt i1 = aEvalExpr e1 s
        AVInt i2 = aEvalExpr e2 s
        aplus Pos Pos = Pos
        aplus Pos Zero = Pos
        aplus Zero Pos = Pos
        aplus Neg Neg = Neg
        aplus Neg Zero = Neg
        aplus Zero Neg = Neg
        aplus Zero Zero = Zero
        aplus _ _ = AnyInt
aEvalExpr (Lt e1 e2) s = AVBool (i1 `alt` i2)
  where AVInt i1 = aEvalExpr e1 s
        AVInt i2 = aEvalExpr e2 s
        alt Neg Zero = ATrue
        alt Neg Pos = ATrue
        alt Zero Pos = ATrue
        alt Zero Neg = AFalse
        alt Pos Neg = AFalse
        alt Pos Zero = AFalse
        alt _ _ = AnyBool

joinAInt :: AInt -> AInt -> AInt
joinAInt x y | x == y = x
             | otherwise = AnyInt

joinABool :: ABool -> ABool -> ABool
joinABool x y | x == y = x
              | otherwise = AnyBool

joinAValue :: AValue -> AValue -> AValue
joinAValue (AVInt x) (AVInt y) = AVInt (joinAInt x y)
joinAValue (AVBool x) (AVBool y) = AVBool (joinABool x y)
joinAValue _ _ = error "type error"

joinAStore :: AStore -> AStore -> AStore
joinAStore = M.unionWith joinAValue

aEvalInstr :: Instr -> AStore -> AStore
aEvalInstr (Assign x e) s = M.insert x (aEvalExpr e s) s
aEvalInstr (If e i1 i2) s =
  case ve of
    ATrue -> aEvalInstr i1 s
    AFalse -> aEvalInstr i2 s
    AnyBool -> aEvalInstr i1 s `joinAStore` aEvalInstr i2 s
  where AVBool ve = aEvalExpr e s
aEvalInstr (While e i) s =
  case ve of
    ATrue -> aEvalInstr (While e i) (aEvalInstr i s)
    AFalse -> s
    AnyBool -> fixpoint i s
  where AVBool ve = aEvalExpr e s
aEvalInstr (Seq i1 i2) s = aEvalInstr i2 (aEvalInstr i1 s)
aEvalInstr Noop s = s

fixpoint i s =
  let s' = aEvalInstr i s
  in if s == s' then s else fixpoint i s'

{- Example -}

block = foldl1 Seq

example =
  block [
    "x" `Assign` IntLit (-1),
    If (Var "x" `Lt` IntLit (-2))
      ("y" `Assign` IntLit 1)
      ("y" `Assign` IntLit 2),
    While (Var "x" `Lt` Var "y")
      (block [
        "x" `Assign` (Var "x" `Plus` IntLit 3),
        "y" `Assign` (Var "y" `Plus` IntLit 1)
      ])
  ]

main = do
  print (evalInstr example emptyStore)
  print (aEvalInstr example emptyAStore)
