#!/usr/bin/env stack
{- stack --resolver lts-8.8 --install-ghc runghc -}

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

import Prelude hiding (id, sequence, all, succ, repeat)
import Control.Monad hiding (sequence)
import Control.Applicative

-- For the sake of simplicity we work on a-terms

data Term = Appl String [Term]
  deriving (Eq, Show)

-- A strategy is a function that takes a term and returns a term or fails

type Strategy = Term -> Maybe Term

-- Below we define the basic strategy combinators in "simle haskell" (we don't
-- use any fancy feature or abstractions, we don't eta-reduce the functions,
-- etc.)

id :: Strategy
id t = Just t

fail :: Strategy
fail t = Nothing

sequence :: Strategy -> Strategy -> Strategy
sequence s1 s2 t =
  case s1 t of
    Just t' -> s2 t'
    Nothing -> Nothing

choice :: Strategy -> Strategy -> Strategy
choice s1 s2 t =
  case s1 t of
    Just t' -> Just t'
    Nothing -> s2 t

all :: Strategy -> Strategy
all s (Appl f ts) =
  case chain (map s ts) of
    Just ts' -> Just (Appl f ts')
    Nothing -> Nothing
  where
    chain :: [Maybe Term] -> Maybe [Term]
    chain [] = Just []
    chain (Just t : mts) =
      case chain mts of
        Just ts -> Just (t : ts)
        Nothing -> Nothing
    chain (Nothing : mts) = Nothing

one :: Strategy -> Strategy
one s (Appl f ts) =
  case first ts of
    Just ts' -> Just (Appl f ts)
    Nothing -> Nothing
  where
    first :: [Term] -> Maybe [Term]
    first [] = Nothing
    first (t : ts) =
      case s t of
        Just t' -> Just (t' : ts)
        Nothing ->
          case first ts of
            Just ts' -> Just (t : ts')
            Nothing -> Nothing

-- Example

try s = choice s id
onceBottomUp s = choice (one (onceBottomUp s)) s
innermost s = sequence (all (innermost s)) (try (sequence s (innermost s)))

zero = Appl "Z" []
succ t = Appl "S" [t]
plus t1 t2 = Appl "plus" [t1, t2]

eval (Appl "plus" [Appl "S" [n], m]) = Just (succ (plus n m))
eval (Appl "plus" [Appl "Z" [], m]) = Just m
eval _ = Nothing

twoPlusOne = plus (succ (succ zero)) (succ zero)

test1 = onceBottomUp eval twoPlusOne
test2 = innermost eval twoPlusOne

-- Below we exploit the fact that Maybe has Monad and Alternative instances to
-- reformulate the basic combinators using haskell standard library functions.

id' :: Strategy
id' = Just

fail' :: Strategy
fail' = const Nothing

sequence' :: Strategy -> Strategy -> Strategy
sequence' = (>=>)

choice' :: Strategy -> Strategy -> Strategy
choice' s1 s2 t = s1 t <|> s2 t

all' :: Strategy -> Strategy
all' s (Appl f ts) = fmap (Appl f) (traverse s ts)

one' :: Strategy -> Strategy
one' s (Appl f ts) = fmap (Appl f) (first ts)
  where
    first [] = Nothing
    first (t:ts) = fmap (:ts) (s t) <|> fmap (t:) (first ts)

try' s = choice' s id'
onceBottomUp' s = choice' (one' (onceBottomUp' s)) s
innermost' s = sequence' (all' (innermost' s)) (try' (sequence' s (innermost' s)))

test1' = onceBottomUp' eval twoPlusOne
test2' = innermost' eval twoPlusOne

-- Display the two tests

main = do
  print test1
  print test2
  print test1'
  print test2'
