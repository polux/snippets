-- Copyright 2015 Google Inc. All Rights Reserved.
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

import qualified Data.Set as S

data Tree = Node { label :: String, children :: [Tree] }

type Edge = (String, String)
type Relation = S.Set Edge

toRelation :: Tree -> Relation
toRelation (Node l ts) = S.map edge (S.fromList (map label ts)) `S.union` (S.unions (map toRelation ts))
  where edge l' = (l, l')

clashes :: Relation -> Relation -> Relation
clashes r1 r2 = (r1 `S.union` r2) `S.difference` (r1 `S.intersection` r2)

pretty :: Relation -> String
pretty rs = unlines (map prettyEdge (S.toList rs))
  where prettyEdge (a, b) = a ++ " -> " ++ b

main = do
  s <- getContents
  putStrLn $ case trees s of
    Nothing -> "parse error"
    Just ((t1, t2), _) -> pretty (clashes (toRelation t1) (toRelation t2))

-- parsing

type Parser a = String -> Maybe (a, String)

success x s = Just (x, s)

andThen :: Parser a -> (a -> Parser b) -> Parser b
andThen p k s = case p s of
                  Nothing -> Nothing
                  Just (x, s') -> k x s'

por p1 p2 s = case p1 s of
                Just x -> Just x
                Nothing -> p2 s

satisfies :: (Char -> Bool) -> Parser Char
satisfies p (c:cs) | p c = Just (c, cs)
satisfies _ _ = Nothing

followedBy p1 p2 = p1 `andThen` (\x -> p2 `andThen` (const (success x)))

char c = satisfies (== c)

letter = satisfies (\c -> c >= 'a' && c <= 'z')

many p = (p `andThen` (\x -> many p `andThen` (\xs -> success (x:xs)))) `por` success []

many1 p = p `andThen` (\x -> many p `andThen` (\xs -> success (x:xs)))

sepBy1 p1 p2 = p1 `andThen` (\x -> many (p2 `andThen` const p1) `andThen` (\xs -> success (x:xs)))

sepBy p1 p2 = sepBy1 p1 p2 `por` success []

whitespace = many (char ' ')

symbol p = p `followedBy` whitespace

lparen = symbol (char '(')
rparen = symbol (char ')')
comma = symbol (char ',')
identifier = symbol (many1 letter)

tree = (identifier           `andThen` (\f ->
        lparen               `andThen` (\_ ->
        (tree `sepBy` comma) `andThen` (\ts ->
        rparen               `andThen` (\_ ->
        success (Node f ts))))))
       `por`
       (identifier `andThen` (\f ->
        success (Node f [])))

trees = symbol tree `andThen` (\t1 -> symbol tree `andThen` (\t2 -> success (t1, t2)))
