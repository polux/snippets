#!/usr/bin/env stack
-- stack runghc --resolver lts-14.17 --package sym

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

import Data.Function
import Data.List
import System.Random
import Sym.Perm (Perm, toList, unrank)
import Sym.Perm.Stat (fp)

candidates n indices =
  map (unrank n) indices
    & filter isDerangement
    & map toList
 where
  isDerangement perm = fp perm == 0

fact :: Integer -> Integer
fact n = product [1..n]

numPeople :: Integer
numPeople = 150

main = do
  gen <- newStdGen
  let indices = randomRs (0, fact numPeople -1) gen
  print (head $ candidates (fromInteger numPeople) indices)
