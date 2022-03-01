module Task4 where

import Type ( Term(Var ,Comb), VarName (VarName) ) 
import Task3 ( Vars(allVars), contains )
import Data.List (delete)

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
compose  subst1 (Subst list2 terms2 )= addit (Subst list2 (map (\t -> apply subst1 t) terms2 )) subst1 
 where 
        addit :: Subst -> Subst -> Subst
        addit (Subst [] []) subst2 = subst2
        addit (Subst vars1 terms1) (Subst vars2 terms2) = Subst (vars1 ++ vars2) (terms1 ++ terms2)
        
                                    

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