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

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

--import Test.SmallCheck.Series
import Test.LazySmallCheck hiding (cons)
import qualified Test.LazySmallCheck as LSC
--import Test.QuickCheck as QC
import Debug.Trace
import Data.List
import Data.Maybe
import Data.Function
import Control.Applicative
import qualified Data.Set as S
import qualified Data.Map as M

type FunctionSymbol = String
type VariableSymbol = String
type Type = String
type Signature = [Decl]
data Decl = Decl { symbol :: FunctionSymbol, range :: Type, domain :: [Type] }
type Constraints = M.Map VariableSymbol (S.Set Term)

data Term = Appl { funName :: FunctionSymbol, children :: [Term] }
          | Var { varName :: VariableSymbol }
 deriving (Eq, Ord)

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
covers _ ps | trace ("covers " ++ show ps) False = undefined
covers sig ps | any isVar ps = True
              | otherwise = all matchesOnePattern closedTermOfMaxDepth
  where typeOfTs = typeOf sig (funName (myHead "covers" (filter isAppl ps)))
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
typeOf sig funName = range (myHead ("typeOf " ++ funName) (filter ((== funName) . symbol) sig))

-- groupChildren [f(t1, t2, t3), f(u1, u2, u3), ...] = [[t1, u1, ...], [t2, u2, ...], [t3, u3, ...]]
groupChildren :: [Term] -> [[Term]]
groupChildren ts = transpose (map children ts)

groupByKey :: Ord k => (a -> k) -> [a] -> [[a]]
groupByKey key xs = M.elems (M.fromListWith (++) (map makeEntry xs))
  where makeEntry x = (key x, [x])

match :: Term -> Term -> Maybe Constraints
match (Appl f ts) (Appl g us) | f == g    = combine (zipWith match ts us)
                              | otherwise = Nothing
  where combine mcs = fmap (M.unionsWith S.union) (sequence mcs)
match (Var x) t = Just M.empty
match t (Var x) = Just (M.singleton x (S.singleton t))

solvable :: Signature -> Constraints -> Bool
solvable sig cs = all (covers sig . S.toList) (M.elems cs)

traceShowId x = traceShow x x

subsumes :: Signature -> [Term] -> Term -> Bool
subsumes _ ps p | trace ("subsumes " ++ show ps ++ " " ++ show p) False = undefined
subsumes sig [] p = False
subsumes sig ps p = subsumes' (catMaybes [match q p | q <- ps])
  where subsumes' [] = False
        subsumes' css | any M.null css = True
                      | otherwise = solvable sig (M.unionsWith S.union css)

minimize sig ps = minimize' ps []
  where minimize' [] kernel = kernel
        minimize' (p:ps) kernel =
           if subsumes sig (ps++kernel) p
              then shortest (minimize' ps (p:kernel)) (minimize' ps kernel)
              else minimize' ps (p:kernel)

        shortest xs ys = if length xs <= length ys then xs else ys

semantics :: Signature -> Int -> Type -> Term -> S.Set Term
semantics sig d ty p = S.fromList $ filter (matches p) (closedTerms sig ty d)

myHead s [] = error s
myHead _ (x:xs) = x

subsumesModel :: Signature -> [Term] -> Term -> Bool
subsumesModel sig [] p = False
subsumesModel sig ps p | all isVar ps = True
                       | otherwise = sem p `S.isSubsetOf` (S.unions (map sem ps))
  where sem = semantics sig dept ty
        ty = typeOf sig (funName (myHead "subsumesModel" (filter isAppl ps)))
        dept = maximum (map depth (p:ps))

linear p = S.size (S.fromList vars) == length vars
  where vars = vars' p
        vars' (Appl f ts) = concat (map vars' ts)
        vars' (Var x) = [x]

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

interp(x, y) = Appl "interp" [x, y]
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
   Decl "false" "Bool" [],
   Decl "interp" "Result" ["Nat", "List"]]

instance Serial Term where
  series = (LSC.cons (curry cons) >< valSeries >< series) \/ cons0 (nil()) \/ varSeries
    where valSeries = (LSC.cons nv >< natSeries) \/ cons0 (undef()) \/ varSeries
          natSeries = (LSC.cons s >< natSeries) \/ cons0 (z()) \/ varSeries
          varSeries = cons0 (Var "x1") \/ cons0 (Var "x2") \/ cons0 (Var "x3")

erase (Appl f ts) = (Appl f (map erase ts))
erase (Var x) = Var "_"

hasVars (Appl _ ts) = any hasVars ts
hasVars (Var _) = True

--property ps p = subsumes example_sig ps p && all linear ps && linear p ==> forAll $ \qs -> subsumes example_sig (ps++qs) p
property ps p = (LSC.lift (linear p) *&* parAll (map (LSC.lift . linear) ps) *&* lift (hasVars p) *&* lift (not (null ps)) *&* lift (any ((>= depth p) . depth) ps)) *=>* lift (subsumesModel example_sig ps p == subsumes example_sig ps p)
--property ps = (length ps > 1 && all linear ps && all isAppl ps) ==> all sameAsResult (map (minimize example_sig) (permutations ps))
  where result = minimize example_sig ps
        sameAsResult r = S.fromList (map erase r) == S.fromList (map erase result)
        parAll = foldr (*&*) (LSC.lift True)

main = depthCheck 4 property
main2 = do
  print (minimize example_sig [nil(), cons(undef(), Var "x1")])
  print (minimize example_sig [cons(undef(), Var "x1"), nil()])


main3 = print (subsumes example_sig example_patterns t)
  where t = interp(s(s(z())), cons(bv(Var "z_38_1"), Var "z_35_2"))


main4 = do
  print (length example_patterns)
  let res = (minimize example_sig example_patterns)
  print (length res)
  putStrLn (unlines (map show res))


main5 = print (subsumes example_sig (example_patterns \\ [t]) t)
  where t = interp(s(s(z())), cons(Var "z_35_1", cons(bv(Var "z_43_1"), Var "z_40_2")))

main6 = putStrLn $ unlines $ map show $ (closedTerms example_sig "List" 2)

main7 = do
  print (length ex_2)
  let res = (minimize sig_2 ex_2)
  print (length res)


ex_3 = [
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), c(Var "z_75_1")),
  numadd(neg(Var "z_94_1"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(bound(Var "z_97_1"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(neg(Var "z_94_1"), c(Var "z_75_1")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), bound(Var "z_79_1")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), add(add(Var "z_29_1", Var "z_29_2"), Var "z_23_2")),
  numadd(add(neg(Var "z_46_1"), Var "z_41_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(bound(Var "z_97_1"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(bound(Var "z_97_1"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), add(mul(Var "z_26_1", sub(Var "z_36_1", Var "z_36_2")), Var "z_23_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), add(sub(Var "z_30_1", Var "z_30_2"), Var "z_23_2")),
  numadd(c(Var "z_93_1"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), add(mul(Var "z_26_1", c(Var "z_33_1")), Var "z_23_2")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), neg(Var "z_76_1")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(sub(Var "z_42_1", Var "z_42_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(c(Var "z_93_1"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), c(Var "z_75_1")),
  numadd(c(Var "z_93_1"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(neg(Var "z_94_1"), neg(Var "z_76_1")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), bound(Var "z_79_1")),
  numadd(c(Var "z_93_1"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(c(Var "z_93_1"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(bound(Var "z_43_1"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), c(Var "z_75_1")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), c(Var "z_75_1")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(neg(Var "z_94_1"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(bound(Var "z_97_1"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), c(Var "z_75_1")),
  numadd(bound(Var "z_97_1"), bound(Var "z_79_1")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), c(Var "z_75_1")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(neg(Var "z_94_1"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(add(add(Var "z_47_1", Var "z_47_2"), Var "z_41_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), neg(Var "z_76_1")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), neg(Var "z_76_1")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), neg(Var "z_76_1")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(neg(Var "z_94_1"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(bound(Var "z_97_1"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), neg(Var "z_76_1")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(bound(Var "z_97_1"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), bound(Var "z_79_1")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), bound(Var "z_79_1")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(neg(Var "z_94_1"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(c(Var "z_93_1"), bound(Var "z_79_1")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), neg(Var "z_76_1")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), bound(Var "z_25_1")),
  numadd(bound(Var "z_97_1"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(c(Var "b1"), c(Var "b2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), bound(Var "z_79_1")),
  numadd(c(Var "z_93_1"), neg(Var "z_76_1")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), c(Var "z_75_1")),
  numadd(bound(Var "z_97_1"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(neg(Var "z_94_1"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(add(mul(Var "z_44_1", mul(Var "z_50_1", Var "z_50_2")), Var "z_41_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(neg(Var "z_94_1"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(bound(Var "z_97_1"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(c(Var "z_39_1"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), neg(Var "z_76_1")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), add(neg(Var "z_28_1"), Var "z_23_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), neg(Var "z_76_1")),
  numadd(c(Var "z_93_1"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(bound(Var "z_49_1"), Var "z_41_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), mul(Var "z_20_1", Var "z_20_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), add(mul(Var "z_26_1", neg(Var "z_34_1")), Var "z_23_2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(neg(Var "z_40_1"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(add(sub(Var "z_48_1", Var "z_48_2"), Var "z_41_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(bound(Var "z_97_1"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(mul(Var "z_38_1", Var "z_38_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), bound(Var "z_79_1")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(neg(Var "z_94_1"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), add(mul(Var "z_26_1", mul(Var "z_32_1", Var "z_32_2")), Var "z_23_2")),
  numadd(neg(Var "z_94_1"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), bound(Var "z_79_1")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(c(Var "z_93_1"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(c(Var "z_45_1"), Var "z_41_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), sub(Var "z_24_1", Var "z_24_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(bound(Var "z_97_1"), c(Var "z_75_1")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), add(bound(Var "z_31_1"), Var "z_23_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), c(Var "z_75_1")),
  numadd(neg(Var "z_94_1"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), neg(Var "z_76_1")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(neg(Var "z_94_1"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(c(Var "z_93_1"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(c(Var "z_93_1"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), add(c(Var "z_27_1"), Var "z_23_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), neg(Var "z_22_1")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(bound(Var "z_97_1"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), c(Var "z_21_1")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(neg(Var "z_94_1"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), c(Var "z_75_1")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), bound(Var "z_79_1")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), c(Var "z_75_1")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), c(Var "z_75_1")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(c(Var "z_93_1"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", neg(Var "z_106_1")), Var "z_95_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(bound(Var "z_97_1"), neg(Var "z_76_1")),
  numadd(add(mul(Var "z_44_1", c(Var "z_51_1")), Var "z_41_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(mul(Var "z_92_1", Var "z_92_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_44_1", neg(Var "z_52_1")), Var "z_41_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), bound(Var "z_79_1")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), mul(Var "z_74_1", Var "z_74_2")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(add(sub(Var "z_102_1", Var "z_102_2"), Var "z_95_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), neg(Var "z_76_1")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), add(sub(Var "z_84_1", Var "z_84_2"), Var "z_77_2")),
  numadd(add(c(Var "z_99_1"), Var "z_95_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(add(add(Var "z_101_1", Var "z_101_2"), Var "z_95_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(bound(Var "z_97_1"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(neg(Var "z_94_1"), bound(Var "z_79_1")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), add(bound(Var "z_85_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), add(mul(Var "z_80_1", sub(Var "z_90_1", Var "z_90_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(c(Var "z_93_1"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), add(mul(Var "z_80_1", add(Var "z_89_1", Var "z_89_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", sub(Var "z_108_1", Var "z_108_2")), Var "z_95_2"), neg(Var "z_76_1")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), add(mul(Var "z_80_1", neg(Var "z_88_1")), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), add(neg(Var "z_82_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_44_1", sub(Var "z_54_1", Var "z_54_2")), Var "z_41_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(add(mul(Var "z_44_1", add(Var "z_53_1", Var "z_53_2")), Var "z_41_2"), add(mul(Var "c2", bound(Var "n2")), Var "r2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), bound(Var "z_79_1")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), bound(Var "z_79_1")),
  numadd(add(mul(Var "z_98_1", add(Var "z_107_1", Var "z_107_2")), Var "z_95_2"), add(add(Var "z_83_1", Var "z_83_2"), Var "z_77_2")),
  numadd(sub(Var "z_96_1", Var "z_96_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), sub(Var "z_78_1", Var "z_78_2")),
  numadd(add(mul(Var "c1", bound(Var "n1")), Var "r1"), add(mul(Var "z_26_1", add(Var "z_35_1", Var "z_35_2")), Var "z_23_2")),
  numadd(add(mul(Var "z_98_1", c(Var "z_105_1")), Var "z_95_2"), add(c(Var "z_81_1"), Var "z_77_2")),
  numadd(add(mul(Var "z_98_1", mul(Var "z_104_1", Var "z_104_2")), Var "z_95_2"), add(mul(Var "z_80_1", c(Var "z_87_1")), Var "z_77_2")),
  numadd(c(Var "z_93_1"), add(mul(Var "z_80_1", mul(Var "z_86_1", Var "z_86_2")), Var "z_77_2")),
  numadd(add(bound(Var "z_103_1"), Var "z_95_2"), neg(Var "z_76_1")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), bound(Var "z_79_1")),
  numadd(add(neg(Var "z_100_1"), Var "z_95_2"), c(Var "z_75_1"))]
    where numadd(x,y) = Appl "numadd" [x,y]
          c(x) = Appl "c" [x]
          bound(x) = Appl "bound" [x]
          neg(x) = Appl "neg" [x]
          add(x, y) = Appl "add" [x, y]
          sub(x, y) = Appl "sub" [x, y]
          mul(x, y) = Appl "mul" [x, y]

sig_2 = [
  Decl "c" "T" ["Nat"],
  Decl "bound" "T" ["Nat"],
  Decl "neg" "T" ["T"],
  Decl "add" "T" ["T", "T"],
  Decl "sub" "T" ["T", "T"],
  Decl "mul" "T" ["T", "T"],
  Decl "s" "Nat" ["Nat"],
  Decl "z" "Nat" []]
