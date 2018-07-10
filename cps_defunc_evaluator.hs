#!/usr/bin/env stack
{-
  stack
  --resolver lts-11.10
  --install-ghc
  runghc
  --package base
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

import qualified Data.Map as M

data Op2 = Minus | Times | Eq
  deriving Show

data Term
  = Var String
  | Lam String Term
  | Rec String Term
  | App Term Term
  | Op2 Op2 Term Term
  | Lit Int
  | If Term Term Term
  deriving Show

type Env = M.Map String Val

data Val
  = VInt Int
  | VBool Bool
  | VClosure Term Env
  | VRClosure String Term Env
  deriving Show

data Frame
  = EvalOp2SecondArg Op2 Term Env
  | EvalOp2 Op2 Val
  | EvalArg Term Env
  | EvalBody String Term Env
  | EvalRecBody Val
  | Branch Term Term Env
  deriving Show

vOp2 Minus (VInt i) (VInt j) = VInt (i - j)
vOp2 Times (VInt i) (VInt j) = VInt (i * j)
vOp2 Eq (VInt i) (VInt j) = VBool (i == j)

apply [] v = v
apply (EvalOp2 op v2 : k) v1 = apply k (vOp2 op v2 v1)
apply (EvalOp2SecondArg op e env : k) v = eval e env (EvalOp2 op v : k)
apply (EvalBody x e env : k) v = eval e (M.insert x v env) k
apply (EvalRecBody vf@(VRClosure f (Lam x e) env) : k) v = eval e (M.insert x v (M.insert f vf env)) k
apply (EvalArg e1 env : k) (VClosure (Lam x e2) env') = eval e1 env (EvalBody x e2 env' : k)
apply (EvalArg e1 env : k) vf@(VRClosure _ _ _) = eval e1 env (EvalRecBody vf : k)
apply (Branch e1 e2 env : k) (VBool True) = eval e1 env k
apply (Branch e1 e2 env : k) (VBool False) = eval e2 env k

eval e env k = eval' e
 where
  eval' (Lit i) = apply k (VInt i)
  eval' (Op2 op e1 e2) = eval e1 env (EvalOp2SecondArg op e2 env : k)
  eval' (App f e) = eval f env (EvalArg e env : k)
  eval' (Lam x e) = apply k (VClosure (Lam x e) env)
  eval' (Rec f (Lam x e)) = apply k (VRClosure f (Lam x e) env)
  eval' (Var x) = apply k (env M.! x)
  eval' (If e1 e2 e3) = eval e1 env (Branch e2 e3 env : k)

fact = Rec
  "fact"
  (Lam
    "n"
    (If (Op2 Eq (Var "n") (Lit 0))
        (Lit 1)
        (Op2 Times (Var "n") (App (Var "fact") (Op2 Minus (Var "n") (Lit 1))))
    )
  )

main = print $ eval (App fact (Lit 20)) M.empty []

