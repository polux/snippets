{- stack runghc --package Earley-0.12.0.0 -}

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

import Text.Earley
import Data.Tree
import Control.Applicative
import Data.Char

treeGrammar :: Grammar r (Prod r String Char (Tree Char))
treeGrammar = mdo
  tree <- rule $  mkNode <$> identifier <*> token '(' <*> arguments <*> token ')'
              <|> mkLeaf <$> identifier
              <?> "tree"
  arguments <- rule $  cons   <$> tree <*> token ',' <*> arguments
                   <|> single <$> tree
                   <|> nil    <$> empty
                   <?> "argument list"
  identifier <- rule $ satisfy isAlpha
                    <?> "identifier"
  return tree
     where
       mkLeaf label = Node label []
       mkNode label _ children _ = Node label children
       cons x _ xs = x:xs
       single x = [x]
       nil _ = []

main = mapM_ print (language (generator treeGrammar "(),ab"))
