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

module Main where

import Test.LazySmallCheck

data Expr = A | B | C | Not Expr | And Expr Expr | Or Expr Expr | Xor Expr Expr
  deriving Show

instance Serial Expr where
  series = cons0 A \/ cons0 B \/ cons0 C \/ cons1 Not \/ cons2 And \/ cons2 Or \/ cons2 Xor

interp a b c = interp'
  where interp' A = lift a
        interp' B = lift b
        interp' C = lift c
        interp' (Not e) = neg (interp' e)
        interp' (And e1 e2) = interp' e1 *&* interp' e2
        interp' (Or e1 e2) = interp' e1 *|* interp' e2
        interp' (Xor e1 e2) = interp' e1 *=* neg (interp' e2)

isNot3Xor e =
      neg (interp True False False e)
  *|* neg (interp False True False e)
  *|* neg (interp False False True e)
  *|* interp False False False e
  *|* interp True True False e
  *|* interp True False True e
  *|* interp False True True e
  *|* interp True True True e

main :: IO ()
main = smallCheck 10 isNot3Xor
