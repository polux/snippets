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


import Debug.Trace
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Set as S
import qualified Data.Map as M

type FunctionSymbol = String
type VariableSymbol = String
type Type = String
type Signature = [Decl]
data Decl = Decl { symbol :: FunctionSymbol, range :: Type, domain :: [Type] }
type Constraints = [(VariableSymbol, Term)]

data Term = Appl { funName :: FunctionSymbol, children :: [Term] }
          | Var { varName :: VariableSymbol }

depth (Var _) = 0
depth (Appl f []) = 1
depth (Appl f ts) = 1 + maximum (map depth ts)

matches :: Term -> Term -> Bool
matches (Appl f ts) (Appl g us) = f == g && and (zipWith matches ts us)
matches (Var _) _ = True
matches _ _ = False

hasRange ty (Decl _ ty' _) = ty == ty'

closedTerms :: Signature -> Type -> Int -> [Term]
closedTerms sig ty n = closedTerms' n ty
  where closedTerms' 0 ty = [Appl f [] | Decl f _ [] <- filter (hasRange ty) sig]
        closedTerms' n ty = do
          Decl f _ tys <- filter (hasRange ty) sig
          ts <- sequence (map (closedTerms' (n-1)) tys)
          return (Appl f ts)

covers :: Signature -> [Term] -> Bool
covers sig ps | all isVar ps = True
              | otherwise = all matchesOnePattern closedTermOfMaxDepth
  where typeOfTs = typeOf sig (funName (head (filter isAppl ps)))
        maxDepth = maximum (map depth ps)
        closedTermOfMaxDepth = closedTerms sig typeOfTs maxDepth
        matchesOnePattern closedTerm = or [matches p closedTerm | p <- ps]

instance Show Term where
  show (Appl f ts) = f ++ "(" ++ intercalate ", " (map show ts) ++ ")"
  show (Var x) = x

sameSet :: [FunctionSymbol] -> [FunctionSymbol] -> Bool
sameSet fs1 fs2 = S.fromList fs1 == S.fromList fs2

isVar :: Term -> Bool
isVar (Var _) = True
isVar _       = False

isAppl :: Term -> Bool
isAppl (Appl _ _) = True
isAppl _ = False

functionsOfType :: Signature -> Type -> [FunctionSymbol]
functionsOfType sig ty = map symbol (filter ((== ty) . range) sig)

typeOf :: Signature -> FunctionSymbol -> Type
typeOf sig funName = range (head (filter ((== funName) . symbol) sig))

-- groupChildren [f(t1, t2, t3), f(u1, u2, u3), ...] = [[t1, u1, ...], [t2, u2, ...], [t3, u3, ...]]
groupChildren :: [Term] -> [[Term]]
groupChildren ts = transpose (map children ts)

groupByKey :: Ord k => (a -> k) -> [a] -> [[a]]
groupByKey key xs = M.elems (M.fromListWith (++) (map makeEntry xs))
  where makeEntry x = (key x, [x])

match :: Term -> Term -> Maybe Constraints
match (Appl f ts) (Appl g us) | f == g    = combine (zipWith match ts us)
                              | otherwise = Nothing
  where combine mcs = fmap concat (sequence mcs)
match (Var x) t = Just []
match t (Var x) = Just [(x, t)]

solvable :: Signature -> Constraints -> Bool
solvable sig cs = all (covers sig) groups
  where groups = map (map snd) (groupByKey fst cs)

subsumes :: Signature -> [Term] -> Term -> Bool
subsumes sig ps p = subsumes' (catMaybes [match q p | q <- ps])
  where subsumes' [] = False
        subsumes' css = solvable sig (concat css)

minimize sig ps = minimize' ps []
  where minimize' [] kernel = kernel
        minimize' (p:ps) kernel =
           if subsumes sig (ps++kernel) p
              then shortest (minimize' ps (p:kernel)) (minimize' ps kernel)
              else minimize' ps (p:kernel)

        shortest xs ys = if length xs <= length ys then xs else ys

example_patterns = [
  interp(s(s(s(s(s(s(s(Var "z_33_1"))))))), Var "y"),
  interp(s(s(s(s(s(s(z())))))), cons(nv(Var "n1"), cons(nv(Var "n2"), nil()))),
  interp(s(s(s(s(s(s(Var "z_69_1")))))), cons(bv(Var "z_38_1"), Var "z_35_2")),
  interp(s(s(s(s(s(s(Var "z_69_1")))))), cons(Var "z_35_1", cons(bv(Var "z_43_1"), Var "z_40_2"))),
  interp(s(s(s(s(s(z()))))), cons(bv(Var "b1"), cons(bv(Var "b2"), nil()))),
  interp(s(s(s(s(s(z()))))), cons(nv(Var "z_51_1"), Var "z_47_2")),
  interp(s(s(s(s(s(z()))))), cons(Var "z_47_1", cons(nv(Var "z_56_1"), Var "z_52_2"))),
  interp(s(s(s(s(s(Var "z_99_1"))))), cons(Var "z_35_1", nil())),
  interp(s(s(s(s(z())))), cons(bv(Var "b"), nil())),
  interp(s(s(s(s(z())))), cons(nv(Var "z_51_1"), Var "z_47_2")),
  interp(s(s(s(s(z())))), cons(Var "z_106_1", cons(Var "z_111_1", Var "z_111_2"))),
  interp(s(s(s(s(Var "z_124_1")))), nil()),
  interp(s(s(s(z()))), cons(Var "z_126_1", Var "z_126_2")),
  interp(s(s(s(z()))), nil()),
  interp(s(s(z())), cons(bv(Var "z_38_1"), Var "z_35_2")),
  interp(s(s(z())), cons(nv(Var "m"), cons(nv(Var "n"), nil()))),
  interp(s(s(z())), cons(Var "z_35_1", cons(bv(Var "z_43_1"), Var "z_40_2"))),
  interp(s(s(z())), cons(Var "z_35_1", nil())),
  interp(s(s(z())), nil()),
  interp(s(s(Var "z_493_1")), cons(nv(Var "z_73_1"), nil())),
  interp(s(z()), cons(bv(Var "z_38_1"), Var "z_35_2")),
  interp(s(z()), cons(nv(Var "n"), nil())),
  interp(s(z()), cons(Var "z_555_1", cons(Var "z_560_1", Var "z_560_2"))),
  interp(s(z()), nil()),
  interp(z(), cons(Var "z_126_1", Var "z_126_2")),
  interp(z(), nil()),
  interp(Var "x", cons(bv(Var "z_38_1"), cons(nv(Var "z_83_1"), Var "z_79_2"))),
  interp(Var "x", cons(nv(Var "z_73_1"), cons(bv(Var "z_43_1"), Var "z_40_2"))),
  interp(Var "x", cons(undef(), Var "z_35_2")),
  interp(Var "x", cons(Var "z_35_1", cons(undef(), Var "z_40_2"))),
  interp(Var "x", cons(Var "z_35_1", cons(Var "z_40_1", cons(Var "z_45_1", Var "z_45_2"))))]
    where interp(x, y) = Appl "interp" [x, y]
          cons(x, y) = Appl "cons" [x, y]
          bv(x) = Appl "bv" [x]
          nv(x) = Appl "nv" [x]
          undef() = Appl "undef" []
          s(x) = Appl "s" [x]
          z() = Appl "z" []
          nil() = Appl "nil" []

example_sig =
  [Decl "cons" "List" ["Val", "List"],
   Decl "nil" "List" [],
   Decl "bv" "Val" ["Bool"],
   Decl "nv" "Val" ["Nat"],
   Decl "undef" "Val" [],
   Decl "s" "Nat" ["Nat"],
   Decl "z" "Nat" [],
   Decl "true" "Bool" [],
   Decl "false" "Bool" []]

main = do
  print (length example_patterns)
  let res = (minimize example_sig example_patterns)
  print (length res)
  putStrLn (unlines (map show res))
