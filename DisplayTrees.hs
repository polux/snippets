#!/usr/bin/env stack
{-
  stack
  --resolver lts-8.0
  --install-ghc
  exec
  --package base
  --package diagrams
  --package diagrams-lib
  --package diagrams-rasterific
  --package testing-feat
  --package diagrams-contrib
  --
  ghc -O2
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
import Diagrams.Backend.Rasterific
import Test.Feat
import Control.Monad
import Text.Printf

deriveEnumerable ''BTree

type BT = BTree ()
type BTT = BTree (BTree ())

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

images :: Int -> [Diagram B]
images n = map normalize imgs
  where imgs = map draw trees
        (V2 w h) = boxExtents (mconcat (map boundingBox imgs))
        bgrect = rect (w+10) (h+11) # fc white # lw 0
        normalize img = img <> bgrect
        draw t = centerXY (drawTreeOfTrees t # bg white)
        (_, trees) = values !! n

main = zipWithM_ render [0::Int ..] (images 19)
  where render i img = renderRasterific (filePath i) size img
        filePath i = printf "/tmp/out_%04d.png" i
        size = mkWidth 600

