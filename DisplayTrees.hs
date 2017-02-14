#!/usr/bin/env stack
{-
  stack
  --resolver lts-8.0
  --install-ghc
  runghc
  --package base
  --package diagrams
  --package diagrams-lib
  --package diagrams-rasterific
  --package testing-feat
  --package diagrams-contrib
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

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs, TemplateHaskell #-}

import Diagrams.Prelude hiding (Empty)
import Diagrams.TwoD.Layout.Tree
import Diagrams.TwoD.Layout.Grid
import Diagrams.TwoD.Text
import Diagrams.Backend.Rasterific.CmdLine
import Test.Feat
import Control.Monad

deriveEnumerable ''BTree

type BT = BTree ()

drawTree :: BTree (Diagram B) -> Diagram B
drawTree tree =
  maybe mempty (renderTree id (~~))
  (symmLayoutBin' (with & slHSep .~ 4 & slVSep .~ 4) tree)
  # lw veryThin

decorateBt :: BTree () -> BTree (Diagram B)
decorateBt (BNode _ l r) =
  BNode (circle 1 # fc white) (decorateBt l) (decorateBt r)
decorateBt Empty = BNode (circle 0.2 # fc black) Empty Empty

decorateBtt :: BTree (BTree ()) -> BTree (Diagram B)
decorateBtt (BNode t l r) =
  BNode (td <> nd) (decorateBtt l) (decorateBtt r)
    where td = scale 0.15 (centerXY (drawTree (decorateBt t)))
          nd = circle 1.5 # fc white
decorateBtt Empty = BNode (circle 0.2 # fc black) Empty Empty

drawTreeOfTrees t = drawTree (decorateBtt t)

treeOfTrees :: BTree (BTree ())
treeOfTrees = select 79 (39533034221095268 * 2 `div` 3)

main = do
  --print (take 100 (zip [0..] (map fst (values :: [(Integer, [BTT])]))))
  defaultMain (drawTreeOfTrees treeOfTrees # bg white)
