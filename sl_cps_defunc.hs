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

data Term = Term String [Term]
  deriving Show

data K
  = Id
  | Choice K K
  | Seq K K
  | All K
  | AllList
  | Try
  | TopDown
  | Repeat
  | One K
  | OneList
  | OnceTopDown
  | OuterMost
  | R
  | MkJust
  | MkNothing
  | TopDownRec K
  | RepeatRec K
  | OnceTopDownRec K
  | ChoiceLeftFailed K Term K K
  | SeqLeftSucceeded K K K
  | AllSubtermsSucceeded String K
  | AllTailSucceeded Term K
  | AllHeadSucceeded K [Term] K K
  | OneSubtermSucceeded String K
  | OneHeadSucceeded [Term] K
  | OneHeadFailed K [Term] K K
  deriving Show


apply Id t sk fk = apply_t sk t
apply (Choice s1 s2) t sk fk = apply s1 t sk (ChoiceLeftFailed s2 t sk fk)
apply (Seq s1 s2) t sk fk = apply s1 t (SeqLeftSucceeded s2 sk fk) fk
apply (All s) (Term f ts) sk fk = apply_ts' AllList s ts (AllSubtermsSucceeded f sk) fk
apply (TopDownRec s) t sk fk = apply (apply_s TopDown s) t sk fk
apply (RepeatRec s) t sk fk = apply (apply_s Repeat s) t sk fk
apply (One s) (Term f ts) sk fk = apply_ts' OneList s ts (OneSubtermSucceeded f sk) fk
apply (OnceTopDownRec s) t sk fk = apply (apply_s OnceTopDown s) t sk fk
apply R (Term "plus" [Term "z" [], m]) sk fk = apply_t sk m
apply R (Term "plus" [Term "s" [n], m]) sk fk = apply_t sk (Term "s" [Term "plus" [n, m]])
apply R t sk fk = apply_u fk ()

apply_u (ChoiceLeftFailed s2 t sk fk) () = apply s2 t sk fk
apply_u MkNothing () = Nothing
apply_u (OneHeadFailed s ts sk fk) () = apply_ts' OneList s ts sk fk

apply_s Try s = Choice s Id
apply_s TopDown s = Seq s (All (TopDownRec s))
apply_s Repeat s = Choice (Seq s (RepeatRec s)) Id
apply_s OnceTopDown s = Choice s (One (OnceTopDownRec s))
apply_s OuterMost s = apply_s Repeat (apply_s OnceTopDown s)

apply_t MkJust x1 = Just x1
apply_t (OneHeadSucceeded ts sk) t = apply_ts sk (t:ts)
apply_t (SeqLeftSucceeded s2 sk fk) t' = apply s2 t' sk fk
apply_t (AllHeadSucceeded s ts sk fk) t = apply_ts' AllList s ts (AllTailSucceeded t sk) fk

apply_ts (AllSubtermsSucceeded f sk) ts = apply_t sk (Term f ts)
apply_ts (AllTailSucceeded t sk) ts = apply_ts sk (t:ts)
apply_ts (OneSubtermSucceeded f sk) ts = apply_t sk (Term f ts)

apply_ts' AllList s [] sk fk = apply_ts sk []
apply_ts' AllList s (t:ts) sk fk = apply s t (AllHeadSucceeded s ts sk fk) fk
apply_ts' OneList s [] sk fk = apply_u fk ()
apply_ts' OneList s (t:ts) sk fk = apply s t (OneHeadSucceeded ts sk) (OneHeadFailed s ts sk fk)

plus a b = Term "plus" [a, b]
z = Term "z" []
s n = Term "s" [n]

main = print $ apply (apply_s OuterMost R) (plus (s (s z)) (s z)) MkJust MkNothing
