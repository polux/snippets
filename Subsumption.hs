-- Copyright 2015 Google Inc. All Rights Reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License")--
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

import Data.String (IsString(..))
import Data.Maybe (fromJust)
import Data.List (find, intercalate)
import qualified Data.Set as S (fromList)

{- Datatypes -}

newtype FunName = FunName String
  deriving (Eq, Ord)

data Pattern = PCons FunName [Pattern] | PVar
  deriving (Eq, Ord)

newtype TypeName = TypeName String
  deriving (Eq, Ord)

data Decl = Decl FunName [TypeName] TypeName
  deriving (Eq)

data Signature = Signature [Decl]
  deriving (Eq)

{- IsString instances -}

instance IsString FunName where
  fromString = FunName

instance IsString TypeName where
  fromString = TypeName

{- Pretty Printing -}

instance Show FunName where
  show (FunName x) = x

instance Show TypeName where
  show (TypeName ty) = ty

parSep :: [String] -> String
parSep ss = "(" ++ intercalate ", " ss ++ ")"

instance Show Pattern where
  show (PCons f ps) = show f ++ parSep (map show ps)
  show PVar = "_"

instance Show Decl where
  show (Decl f tys ty) = show f ++ ": " ++ parSep (map show tys) ++ " -> " ++ show ty

instance Show Signature where
  show (Signature decls) = show decls

{- Signature Query Functions -}

decl :: Signature -> FunName -> Decl
decl (Signature sig) f = fromJust (find hasF sig)
  where hasF (Decl g _ _) = f == g

domain :: Signature -> FunName -> [TypeName]
domain sig f = domain' (decl sig f)
  where domain' (Decl _ d _) = d

range :: Signature -> FunName -> TypeName
range sig f = range' (decl sig f)
  where range' (Decl _ _ r) = r

arity :: Signature -> FunName -> Int
arity sig f = length (domain sig f)

functionsOfRange :: Signature -> TypeName -> [FunName]
functionsOfRange (Signature sig) ty = map funName (filter hasRangeTy sig)
  where hasRangeTy (Decl _ _ ty') = ty == ty'
        funName (Decl f _ _) = f

functionsOfSameRange :: Signature -> FunName -> [FunName]
functionsOfSameRange sig f = functionsOfRange sig (range sig f)

{- Maranget's Algo -}

type Vector = [Pattern]
type Matrix = [Vector]

sameSet :: Ord a => [a] -> [a] -> Bool
sameSet xs ys = S.fromList xs == S.fromList ys

startsWithVar :: Vector -> Bool
startsWithVar (PVar : _) = True
startsWithVar _ = False

startsWithCons :: Vector -> Bool
startsWithCons (PCons _ _ : _) = True
startsWithCons _ = False

headConstructor :: Vector -> FunName
headConstructor (PCons c _ : _) = c

headConstructors :: Matrix -> [FunName]
headConstructors m = map headConstructor (filter startsWithCons m)

subsumes :: Signature -> [Pattern] -> Pattern -> Bool
subsumes sig ps p = useless [[q] | q <- ps] [p]

  where

    useless [] _ = False
    useless _ [] = True
    useless m v@(PCons c _ : _) = useless (specializeM c m) (specializeV c v)
    useless m v@(PVar : ps)
      | complete = and [useless (specializeM c m) (specializeV c v) | c <- possibleCtors]
      | otherwise = useless (defaultM m) ps
      where
        complete = not (null headCtors) && sameSet headCtors possibleCtors
        headCtors = headConstructors m
        possibleCtors = functionsOfSameRange sig (head headCtors)

    specializeM c m = map (specializeV c) (filter keep m)
       where keep v = startsWithVar v || (headConstructor v == c)

    specializeV c (PCons _ ps : qs) = ps ++ qs
    specializeV c (PVar : qs) = replicate (arity sig c) PVar ++ qs

    defaultM m = map tail (filter startsWithVar m)

{- Minimization Algo -}

minimize sig ps = minimize' ps []
  where minimize' [] kernel = kernel
        minimize' (p:ps) kernel =
           if subsumes sig (ps++kernel) p
              then shortest (minimize' ps (p:kernel)) (minimize' ps kernel)
              else minimize' ps (p:kernel)

        shortest xs ys = if length xs <= length ys then xs else ys

{- Demo -}

signature = Signature
  [ Decl "cons" ["Val", "List"] "List"
  , Decl "nil" [] "List"
  , Decl "bv" ["Bool"] "Val"
  , Decl "nv" ["Nat"] "Val"
  , Decl "undef" [] "Val"
  , Decl "s" ["Nat"] "Nat"
  , Decl "z" [] "Nat"
  , Decl "true" [] "Bool"
  , Decl "false" [] "Bool"
  , Decl "interp" ["Nat", "List"] "Result" ]

_x = PVar
cons(x, y) = PCons "cons" [x, y]
nil() = PCons "nil" []
bv(x) = PCons "bv" [x]
nv(x) = PCons "nv" [x]
undef() = PCons "undef" []
s(x) = PCons "s" [x]
z() = PCons "z" []
interp(x, y) = PCons "interp" [x, y]

patterns =
  [ interp(s(s(s(s(s(s(s(_x))))))), _x)
  , interp(s(s(s(s(s(s(z())))))), cons(nv(_x), cons(nv(_x), nil())))
  , interp(s(s(s(s(s(s(_x)))))), cons(bv(_x), _x))
  , interp(s(s(s(s(s(s(_x)))))), cons(_x, cons(bv(_x), _x)))
  , interp(s(s(s(s(s(z()))))), cons(bv(_x), cons(bv(_x), nil())))
  , interp(s(s(s(s(s(z()))))), cons(nv(_x), _x))
  , interp(s(s(s(s(s(z()))))), cons(_x, cons(nv(_x), _x)))
  , interp(s(s(s(s(s(_x))))), cons(_x, nil()))
  , interp(s(s(s(s(z())))), cons(bv(_x), nil()))
  , interp(s(s(s(s(z())))), cons(nv(_x), _x))
  , interp(s(s(s(s(z())))), cons(_x, cons(_x, _x)))
  , interp(s(s(s(s(_x)))), nil())
  , interp(s(s(s(z()))), cons(_x, _x))
  , interp(s(s(s(z()))), nil())
  , interp(s(s(z())), cons(bv(_x), _x))
  , interp(s(s(z())), cons(nv(_x), cons(nv(_x), nil())))
  , interp(s(s(z())), cons(_x, cons(bv(_x), _x)))
  , interp(s(s(z())), cons(_x, nil()))
  , interp(s(s(z())), nil())
  , interp(s(s(_x)), cons(nv(_x), nil()))
  , interp(s(z()), cons(bv(_x), _x))
  , interp(s(z()), cons(nv(_x), nil()))
  , interp(s(z()), cons(_x, cons(_x, _x)))
  , interp(s(z()), nil())
  , interp(z(), cons(_x, _x))
  , interp(z(), nil())
  , interp(_x, cons(bv(_x), cons(nv(_x), _x)))
  , interp(_x, cons(nv(_x), cons(bv(_x), _x)))
  , interp(_x, cons(undef(), _x))
  , interp(_x, cons(_x, cons(undef(), _x)))
  , interp(_x, cons(_x, cons(_x, cons(_x, _x)))) ]

result = minimize signature patterns

main = do
  putStrLn ("length of patterns: " ++ show (length patterns))
  putStrLn ("length of minimize signature patterns: " ++ show (length result))
  putStrLn ""
  putStrLn (unlines (map show result))
