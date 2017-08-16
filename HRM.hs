#!/usr/bin/env stack
{-
  stack
  --resolver lts-9.0
  --install-ghc
  runghc
  --package base
  --package containers
  --package array
  --package transformers
  --package microlens
  --package microlens-th
  --package microlens-ghc
  --package pretty-show
  --
  -hide-all-packages
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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.List
import Data.Maybe
import Data.Ix
import Text.Show.Pretty
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array as A
import Control.Monad.Trans.Writer
import Lens.Micro
import Lens.Micro.GHC
import Lens.Micro.TH

type Address = Int
type Label = Int

data Instruction
  = Inbox
  | Outbox
  | CopyTo Address
  | CopyFrom Address
  | CopyToAt Address
  | CopyFromAt Address
  | Add Address
  | Sub Address
  | AddAt Address
  | SubAt Address
  | BumpUp Address
  | BumpDown Address
  | BumpUpAt Address
  | BumpDownAt Address
  | Label Label
  | Jump Label
  | JumpZero Label
  | JumpNeg Label
  deriving (Show)

inbox :: Writer [Instruction] ()
inbox = tell [Inbox]

outbox :: Writer [Instruction] ()
outbox = tell [Outbox]

copyTo :: Address -> Writer [Instruction] ()
copyTo address = tell [CopyTo address]

copyFrom :: Address -> Writer [Instruction] ()
copyFrom address = tell [CopyFrom address]

copyToAt :: Address -> Writer [Instruction] ()
copyToAt address = tell [CopyToAt address]

copyFromAt :: Address -> Writer [Instruction] ()
copyFromAt address = tell [CopyFromAt address]

add :: Address -> Writer [Instruction] ()
add address = tell [Add address]

sub :: Address -> Writer [Instruction] ()
sub address = tell [Sub address]

addAt :: Address -> Writer [Instruction] ()
addAt address = tell [AddAt address]

subAt :: Address -> Writer [Instruction] ()
subAt address = tell [SubAt address]

bumpUp :: Address -> Writer [Instruction] ()
bumpUp address = tell [BumpUp address]

bumpDown :: Address -> Writer [Instruction] ()
bumpDown address = tell [BumpDown address]

bumpUpAt :: Address -> Writer [Instruction] ()
bumpUpAt address = tell [BumpUpAt address]

bumpDownAt :: Address -> Writer [Instruction] ()
bumpDownAt address = tell [BumpDownAt address]

label :: Label -> Writer [Instruction] ()
label label = tell [Label label]

(=:) :: Label -> Writer [Instruction] () -> Writer [Instruction] ()
l =: code = do { label l ; code }

jump :: Label -> Writer [Instruction] ()
jump label = tell [Jump label]

jumpZero :: Label -> Writer [Instruction] ()
jumpZero label = tell [JumpZero label]

jumpNeg :: Label -> Writer [Instruction] ()
jumpNeg label = tell [JumpNeg label]

data Machine = Machine
  { _instructions :: A.Array Int Instruction
  , _pc :: Int
  , _register :: Maybe Int
  , _memory :: A.Array Address (Maybe Int)
  , _input :: [Int]
  , _output :: [Int]
  }
  deriving (Show)

makeLenses ''Machine

data Error
  = Overflow Address
  | Underflow Address
  | ReadFromEmptyCell
  | ReadFromEmptyRegister
  | ReadFromEmptyInbox
  deriving (Show)

makeMachine :: Int -> [(Address, Int)] -> [Instruction] -> [Int] -> Machine
makeMachine numCells initialValues instructions input =
  Machine
    { _instructions = (A.listArray (0, length instructions - 1) instructions)
    , _pc = 0
    , _register = Nothing
    , _memory = A.array
        (0, maxIdx)
        [(i, lookup i initialValues) | i <- [0..maxIdx]]
    , _input = input
    , _output = []
    }
  where maxIdx = numCells - 1

maxValue = 999
minValue = -999

allDiff xs = length (S.fromList xs) == length xs

wellFormed :: Machine -> Bool
wellFormed Machine{..} =
  all inRange (catMaybes (A.elems _memory))
  && all inRange _input
  && all (< numCells) addresses
  && allDiff labels
  && (S.fromList references) `S.isSubsetOf` (S.fromList labels)

  where instructionList = A.elems _instructions
        numCells = length _instructions
        labels = [label | Label label <- instructionList]
        addresses = concatMap address instructionList
        address (CopyTo a) = [a]
        address (CopyFrom a) = [a]
        address (CopyToAt a) = [a]
        address (CopyFromAt a) = [a]
        address (Add a) = [a]
        address (Sub a) = [a]
        address (AddAt a) = [a]
        address (SubAt a) = [a]
        address (BumpUp a) = [a]
        address (BumpDown a) = [a]
        address (BumpUpAt a) = [a]
        address (BumpDownAt a) = [a]
        address _ = []
        references = concatMap reference instructionList
        reference (Jump l) = [l]
        reference (JumpZero l) = [l]
        reference (JumpNeg l) = [l]
        reference _ = []
        inRange i = minValue <= i && i <= maxValue

numCommands Machine{..} = sum (map cost (A.elems _instructions))
  where cost (Label _) = 0
        cost _ = 1

findLabelIndex instructions l = find isLabel (range (A.bounds instructions))
  where isLabel i =
          case instructions A.! i of
            Label l' -> l' == l
            _ -> False

step :: Machine -> Either Error Machine
step m@Machine{..} =
  case _instructions A.! _pc of
    Label _ ->
      Right $ m & pc %~ succ
    Jump l ->
      Right $ m & pc .~ findLabel l
    JumpZero l ->
      case _register of
        Just i -> Right $
          m & pc .~ (if i == 0 then findLabel l else succ _pc)
        Nothing -> Left ReadFromEmptyRegister
    JumpNeg l ->
      case _register of
        Just i -> Right $
          m & pc .~ (if i < 0 then findLabel l else succ _pc)
        Nothing -> Left ReadFromEmptyRegister
    Inbox ->
      case _input of
        (x:xs) -> Right $
          m & register .~ Just x
            & input .~ xs
            & pc %~ succ
        [] -> Left ReadFromEmptyInbox
    Outbox ->
      case _register of
        Just x -> Right $
          m & register .~ Nothing
            & output %~ (x:)
            & pc %~ succ
    CopyTo a ->
      case _register of
        Just i -> Right $
          m & memory . ix a .~ Just i
            & pc %~ succ
        Nothing -> Left ReadFromEmptyRegister
    CopyFrom a ->
      case _memory A.! a of
        Just i -> Right $
          m & register .~ Just i
            & pc %~ succ
        Nothing -> Left ReadFromEmptyCell
    Add a ->
      case _memory A.! a of
        Just j ->
          case _register of
            Just i -> Right $
              m & register .~ Just (i+j)
                & pc %~ succ
            Nothing -> Left ReadFromEmptyRegister
        Nothing -> Left ReadFromEmptyCell
    Sub a ->
      case _memory A.! a of
        Just j ->
          case _register of
            Just i -> Right $
              m & register .~ Just (i-j)
                & pc %~ succ
            Nothing -> Left ReadFromEmptyRegister
        Nothing -> Left ReadFromEmptyCell
    _ ->
      error "not implemented"

  where findLabel l = fromJust (findLabelIndex _instructions l)

run :: Machine -> [Either Error Machine]
run m = case step m of
          Left e -> [Left e]
          Right m' -> Right m' : run m'

threeSort = makeMachine 4 [] program input
  where input = [3,1,34,5,1,7,5,9,7,2,78,9]
        -- names
        tmp = 3
        -- labels
        loop = 0
        l1 = 1
        l2 = 2
        l3 = 3
        -- macros
        swap i j t = do
          copyFrom i
          copyTo t
          copyFrom j
          copyTo i
          copyFrom t
          copyTo j
        read a = do
          inbox
          copyTo a
        write a = do
          copyFrom a
          outbox
        compare a b = do
          copyFrom a
          sub b
        -- main
        program = execWriter $ do
          loop =: do
            read 0
            read 1
            read 2
          l1 =: do
            compare 0 1
            jumpNeg l2
            swap 0 1 tmp
          l2 =: do
            compare 1 2
            jumpNeg l3
            swap 1 2 tmp
            jump l1
          l3 =: do
            write 0
            write 1
            write 2
            jump loop

main :: IO ()
main = do
  print (wellFormed threeSort)
  print (numCommands threeSort)
  mapM_ pPrint (run threeSort)
