-- Copyright 2023 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Control.Monad (guard)
import Data.Bits (setBit, (.&.), (.|.))
import Data.Char (isAscii, isLower, ord)
import Data.Function ((&))
import Data.List (tails, sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word (Word32)
import Debug.Trace (traceShow, traceShowId)

type Mask = Word32

toMask :: String -> Mask
toMask = foldr set 0
 where
  set :: Char -> Mask -> Mask
  set c mask = setBit mask (ord c - ord 'a')

loadWords :: IO [(String, Mask)]
loadWords = do
  s <- readFile "words_alpha.txt"
  return $
    [ (word, toMask word)
    | word <- map init (lines s)
    , length word == 5
    , all isAscii word
    , allDiff word
    ]
 where
  allDiff [] = True
  allDiff (c : cs) = c `notElem` cs && allDiff cs

find :: [Mask] -> [[Mask]]
find masks = go 0 masks 5
 where
  go :: Mask -> [Mask] -> Int -> [[Mask]]
  go seen masks 0 = return []
  go seen masks n = do
    (mask : masks) <- tails masks
    guard (seen .&. mask == 0)
    rest <- go (seen .|. mask) masks (n - 1)
    return (mask : rest)

dedupe :: [Mask] -> [Mask]
dedupe = S.toList . S.fromList

toReverseMap :: [(String, Mask)] -> Map Mask String
toReverseMap ws = M.fromListWith const [(m, s) | (s, m) <- ws]

main :: IO ()
main = do
  words <- loadWords
  let masks = map snd words
  let reverseMap = toReverseMap words
  dedupe masks
    & find
    & head
    & map (reverseMap M.!)
    & print
