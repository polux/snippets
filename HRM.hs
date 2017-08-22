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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.List
import Data.Maybe
import Data.Ix
import Text.Show.Pretty
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array as A
import Control.Monad.Fix
import Control.Monad.Trans.RWS
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
  | Jump Label
  | JumpZero Label
  | JumpNeg Label
  deriving (Show)

newtype HRM a = HRM (RWS () [Instruction] Label a)
  deriving (Functor, Applicative, Monad, MonadFix)

writeInstruction :: Instruction -> HRM ()
writeInstruction instr = HRM $ do
  tell [instr]
  modify succ

runHRM :: HRM () -> [Instruction]
runHRM (HRM m) = let (_, _, is) = runRWS m () 0 in is

inbox :: HRM ()
inbox = writeInstruction Inbox

outbox :: HRM ()
outbox = writeInstruction Outbox

copyTo :: Address -> HRM ()
copyTo address = writeInstruction (CopyTo address)

copyFrom :: Address -> HRM ()
copyFrom address = writeInstruction (CopyFrom address)

copyToAt :: Address -> HRM ()
copyToAt address = writeInstruction (CopyToAt address)

copyFromAt :: Address -> HRM ()
copyFromAt address = writeInstruction (CopyFromAt address)

add :: Address -> HRM ()
add address = writeInstruction (Add address)

sub :: Address -> HRM ()
sub address = writeInstruction (Sub address)

addAt :: Address -> HRM ()
addAt address = writeInstruction (AddAt address)

subAt :: Address -> HRM ()
subAt address = writeInstruction (SubAt address)

bumpUp :: Address -> HRM ()
bumpUp address = writeInstruction (BumpUp address)

bumpDown :: Address -> HRM ()
bumpDown address = writeInstruction (BumpDown address)

bumpUpAt :: Address -> HRM ()
bumpUpAt address = writeInstruction (BumpUpAt address)

bumpDownAt :: Address -> HRM ()
bumpDownAt address = writeInstruction (BumpDownAt address)

label :: HRM Label
label = HRM get

jump :: Label -> HRM ()
jump label = writeInstruction (Jump label)

jumpZero :: Label -> HRM ()
jumpZero label = writeInstruction (JumpZero label)

jumpNeg :: Label -> HRM ()
jumpNeg label = writeInstruction (JumpNeg label)

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
  | ReadOutOfRange
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

wellFormed Machine{..} =
  all inRange (catMaybes (A.elems _memory))
  && all inRange _input
  && all (< numCells) addresses

  where instructionList = A.elems _instructions
        numCells = length _instructions
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
        inRange i = minValue <= i && i <= maxValue

numCommands Machine{..} = length _instructions

step :: Machine -> Either Error Machine
step m@Machine{..} =
  case _instructions A.! _pc of
    Jump l ->
      Right $ m & pc .~ l
    JumpZero l -> do
      i <- readRegister
      return $ m & pc .~ (if i == 0 then l else succ _pc)
    JumpNeg l -> do
      i <- readRegister
      return $ m & pc .~ (if i < 0 then l else succ _pc)
    Inbox ->
      case _input of
        (x:xs) -> Right $
          m & register .~ Just x
            & input .~ xs
            & pc %~ succ
        [] -> Left ReadFromEmptyInbox
    Outbox -> do
      i <- readRegister
      return $
        m & register .~ Nothing
          & output %~ (i:)
          & pc %~ succ
    CopyTo a -> do
      i <- readRegister
      return $
        m & memory . ix a .~ Just i
          & pc %~ succ
    CopyFrom a -> do
      i <- readAt a
      return $
        m & register .~ Just i
          & pc %~ succ
    CopyToAt a -> do
      i <- readRegister
      b <- readAt a
      return $
        m & memory . ix b .~ Just i
          & pc %~ succ
    CopyFromAt a -> do
      b <- readAt a
      i <- readAt b
      return $
        m & register .~ Just i
          & pc %~ succ
    Add a -> do
      i <- readRegister
      j <- readAt a
      return $
        m & register .~ Just (i+j)
          & pc %~ succ
    Sub a -> do
      i <- readRegister
      j <- readAt a
      return $
        m & register .~ Just (i-j)
          & pc %~ succ
    AddAt a -> do
      i <- readRegister
      b <- readAt a
      j <- readAt b
      return $
        m & register .~ Just (i+j)
          & pc %~ succ
    SubAt a -> do
      i <- readRegister
      b <- readAt a
      j <- readAt b
      return $
        m & register .~ Just (i-j)
          & pc %~ succ
    BumpUp a -> do
      i <- readAt a
      let v = Just (succ i)
      return $
        m & register .~ v
          & memory . ix a .~ v
          & pc %~ succ
    BumpDown a -> do
      i <- readAt a
      let v = Just (pred i)
      return $
        m & register .~ v
          & memory . ix a .~ v
          & pc %~ succ
    BumpUpAt a -> do
      b <- readAt a
      i <- readAt b
      let v = Just (succ i)
      return $
        m & register .~ v
          & memory . ix b .~ v
          & pc %~ succ
    BumpDownAt a -> do
      b <- readAt a
      i <- readAt b
      let v = Just (pred i)
      return $
        m & register .~ v
          & memory . ix b .~ v
          & pc %~ succ

  where inRange a = a >= low && a <= high

        (low, high) = A.bounds _memory

        readRegister =
          case _register of
            Just i -> Right i
            Nothing -> Left ReadFromEmptyRegister

        readAt a =
          if inRange a
            then case _memory A.! a of
              Just i -> Right i
              Nothing -> Left ReadFromEmptyCell
            else Left ReadOutOfRange

run :: Machine -> [Either Error Machine]
run m = case step m of
          Left e -> [Left e]
          Right m' -> Right m' : run m'

sort = makeMachine 25 [(zero, 0)] (runHRM program) input
  where input = [2,3,2,4,1,0,1,0]
        -- names
        zero = 24
        tmp = 23
        i = 22
        j = 21
        predJ = 20
        -- macros
        swapAt a1 a2 tmp = do
          copyFromAt a1
          copyTo tmp
          copyFromAt a2
          copyToAt a1
          copyFrom tmp
          copyToAt a2
        -- main
        program = mdo
          init <- label
          do copyFrom zero
             copyTo i
             bumpDown i
          start <- label
          do bumpUp i
             inbox
             jumpZero flush
             copyToAt i
             copyFrom i
             copyTo j
             copyTo predJ
             bumpDown predJ
          insert <- label
          do jumpNeg start
             copyFromAt j
             subAt predJ
             jumpNeg start
             swapAt j predJ tmp
             bumpDown j
             bumpDown predJ
             jump insert
          flush <- label
          do bumpDown i
             jumpNeg start
             copyFromAt i
             outbox
             jump flush

main :: IO ()
main = do
  let trace = run sort
  mapM_ pPrint trace
  print (wellFormed sort)
  print (numCommands sort)
  print (length trace)
