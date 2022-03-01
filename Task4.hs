{-# LANGUAGE TemplateHaskell #-}
module Task4 where

import Type ( Goal(Goal), Term(Var, Comb), VarName(VarName)) 
import Task3 ( Vars(..), contains, removeDuplikates ) 
import Task2 ( Pretty(..) ) 
import Data.List (delete)
import Test.QuickCheck (Arbitrary (arbitrary), quickCheckAll, Gen, choose)
import System.Win32 (xBUTTON1)

data Subst = Subst [VarName] [Term] 
 deriving Show

domain :: Subst -> [VarName]
domain (Subst [] []) = []
domain (Subst (v:vs) ((Var x):ts)) = if( v == x ) 
                                      then domain (Subst vs ts) 
                                      else (v:domain (Subst vs ts))
domain (Subst (v:vs) (t:ts)) = (v:domain (Subst vs ts))
domain _ = []

empty :: Subst
empty = Subst [] []

single :: VarName -> Term -> Subst
single name (Var s) = if ( name == s ) then empty else Subst [name] [Var s]
single name term = Subst [name] [term]

apply :: Subst -> Term -> Term
apply (Subst [] [] ) term2 = term2
apply (Subst (n:ns) (t:ts)) (Var x) = if(n == x) then t else apply (Subst ns ts) (Var x)
apply subst (Comb s terms) = Comb s (map (\x -> apply subst x) terms)
apply _ _ = (Var (VarName "A"))


compose :: Subst -> Subst -> Subst
compose  subst1 (Subst list2 terms2) = add (Subst list2 (map (\t -> apply subst1 t) terms2 )) subst1 
 where 
        add :: Subst -> Subst -> Subst
        add subst2 (Subst [] []) = subst2
        add (Subst vars1 terms1) (Subst (v2:v2s) (t2:t2s)) =
            if ( contains v2 vars1 ) 
                then add (Subst vars1  terms1) (Subst v2s t2s) 
                else add (Subst (vars1 ++ [v2]) (terms1 ++ [t2])) (Subst v2s t2s)
        add _ _ = empty
       
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst [] []) _ = (Subst [] [])
restrictTo (Subst (v:vs) (t:ts)) varlist = if(contains v varlist )
                                             then compose (Subst [v] [t]) (restrictTo (Subst vs ts) varlist) 
                                             else (restrictTo (Subst vs ts) varlist)
restrictTo _ _ = empty
       


instance Pretty Subst where 
    pretty (Subst [][]) = "{}"
    pretty subst = "{" ++ recPretty subst ++ "}" 
     where 
         recPretty :: Subst -> String
         recPretty (Subst [] []) = ""
         recPretty (Subst [var] [term]) = if( [var] == allVars term) then "" else pretty (Var var) ++ " -> " ++  pretty term
         recPretty (Subst (v:vs) (t:ts)) =  if( [v] == allVars t) 
                                                then recPretty (Subst vs ts) 
                                                else pretty (Var v) ++ " -> " ++ pretty t ++ ", " ++ recPretty (Subst vs ts)
         recPretty _ = ""

instance Vars Subst where
    allVars (Subst vars terms) = removeDuplikates ( vars ++ allVars (Goal terms) )


instance Test.QuickCheck.Arbitrary Subst where
 arbitrary = do 
                vars <- Test.QuickCheck.arbitrary  
                terms <- Test.QuickCheck.arbitrary 
                x <- choose ( 0, getSmalerLength vars terms 0 )
                return (Subst (take x vars) (take x terms))




------------------------------- Helperfunctions ---------------------------------------------
getSmalerLength :: [a] -> [b] -> Int -> Int 
getSmalerLength [] _ x = x
getSmalerLength _ [] x = x
getSmalerLength (_:as) (_:bs) x = getSmalerLength as bs (x+1)

--           this without this   
without ::  [VarName] -> [VarName] -> [VarName] 
without list [] = list
without list (r:rs) = if (contains r list) 
                            then without (delete r list) rs 
                            else without list rs

--  true if      this  is in  this 
allContains :: [VarName] -> [VarName] -> Bool
allContains []  _ = True
allContains (x:xs) list = if( contains x list ) then allContains xs list else False 

setEq :: [VarName] -> [VarName] -> Bool 
setEq a b = allContains a b && allContains b a

--------------------------------- Automatic Tests -----------------------------------------------

prop_test1 :: Term -> Bool
prop_test1 t = apply empty t == t
prop_test2 :: VarName -> Term -> Bool 
prop_test2 x t =  apply(single x t) (Var x) == t
prop_test3 :: Term -> Subst -> Subst -> Bool
prop_test3 t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)
prop_test4 :: Bool
prop_test4 = domain empty == []
prop_test5 :: VarName -> Bool
prop_test5 x = domain (single x (Var x)) == []
prop_test6 :: VarName -> Term -> Bool
prop_test6 x t = if(t /= (Var x)) then domain (single x t) == [x]
                                  else True
prop_test7 :: Subst -> Subst -> Bool
prop_test7 s1 s2 = allContains (domain (compose s1 s2)) (domain s1 ++ domain s2)
prop_test8 :: VarName -> VarName -> Bool
prop_test8 x1 x2 = if(x1 /= x2) then domain (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2]
                                else True 
prop_test9 :: Bool
prop_test9 = allVars empty == []
prop_test10 ::VarName -> Bool
prop_test10 x = allVars (single x (Var x)) == []
prop_test11 :: VarName -> Term -> Bool 
prop_test11 x t = if(t /= (Var x)) then setEq (allVars (single x t)) ([x] ++ (allVars t)) 
                                   else True
prop_test12 :: Subst -> Subst -> Bool
prop_test12 s1 s2 = allContains(allVars (compose s1 s2)) (allVars s1 ++ allVars s2)
prop_test13 :: VarName -> VarName -> Bool
prop_test13 x1 x2 = if(x1 /= x2) then setEq (allVars (compose (single x2 (Var x1)) (single x1 (Var x2)))) [x1,x2] 
                                 else True
prop_test14 :: Subst -> Bool
prop_test14 s = allContains (domain s) (allVars s)
prop_test15 :: [VarName] -> Bool
prop_test15 xs = domain (restrictTo empty xs) == []
prop_test16 :: [VarName] -> Subst -> Bool
prop_test16 xs s = allContains (domain(restrictTo s xs )) xs
return []

runTests :: IO Bool
runTests = $quickCheckAll

