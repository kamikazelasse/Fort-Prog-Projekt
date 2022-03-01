module Task4 where

import Type ( Goal(Goal), Term(..), VarName (VarName) ) 
import Task3 ( Vars(..), contains, removeDuplikates ) 
import Task2 ( Pretty(..) ) 
import Data.List (delete)
import Test.QuickCheck (Arbitrary (arbitrary))

data Subst = Subst [VarName] [Term] 
 deriving Show

domain :: Subst -> [VarName]
domain (Subst [] []) = []
domain (Subst (v:vs) (t:ts)) = if( [v] == allVars t ) 
                                  then domain (Subst vs ts) 
                                  else (v:domain (Subst vs ts))

empty :: Subst
empty = Subst [] []

single :: VarName -> Term -> Subst
single name term = Subst [name] [term]

apply :: Subst -> Term -> Term
apply (Subst [] [] ) term2 = term2
apply (Subst (n:ns) (t:ts)) (Var x) = if(n == x) then t else apply (Subst ns ts) (Var x)
apply subst (Comb s terms) = Comb s (map (\x -> apply subst x) terms)


compose :: Subst -> Subst -> Subst
compose  subst1 (Subst list2 terms2 )= add (Subst list2 (map (\t -> apply subst1 t) terms2 )) subst1 
 where 
        add :: Subst -> Subst -> Subst
        add subst1 (Subst [] []) = subst1
        add (Subst vars1 terms1) (Subst (v2:v2s) (t2:t2s)) =
            if ( contains v2 vars1 ) 
                then add (Subst vars1  terms1) (Subst v2s t2s) 
                else add (Subst (vars1 ++ [v2]) (terms1 ++ [t2])) (Subst v2s t2s) 
       
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst [] []) varlist = (Subst [] [])
restrictTo (Subst (v:vs) (t:ts)) varlist = if(contains v varlist )
                                             then compose (Subst [v] [t]) (restrictTo (Subst vs ts) varlist) 
                                             else (restrictTo (Subst vs ts) varlist)
       


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

instance Vars Subst where
    allVars (Subst vars terms) = removeDuplikates ( vars ++ allVars (Goal terms) )


instance Test.QuickCheck.Arbitrary Subst where
 arbitrary = do vars <- Test.QuickCheck.arbitrary  
                terms <- Test.QuickCheck.arbitrary  
                return (Subst vars terms)



------------------------------- not Helperfunktions ---------------------------------------------

--           this without this   
without ::  [VarName] -> [VarName] -> [VarName] 
without list [] = list
without list (r:rs) = if (contains r list) 
                            then without (delete r list) rs 
                            else without list rs

--  true if    this \/ is in this \/
allContains :: [VarName] -> [VarName] -> Bool
allContains []  _ = True
allContains (x:xs) list = if( contains x list ) then allContains xs list else False 