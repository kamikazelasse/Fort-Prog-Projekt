module Task4 where

import Type ( Term(Comb), VarName ) 
import Task3 ( Vars(allVars), contains )
import Data.List (delete)

data Subst = Subst [VarName] Term 

domain :: Subst -> [VarName]
domain (Subst vars term) =  without vars (allVars term)

empty :: Subst
empty = Subst [] (Comb "" []) -- ?

single :: VarName -> Term -> Subst
single name term = Subst [name] term

apply :: Subst -> Term -> Term
apply (Subst [] _ ) term2 = term2
apply (Subst list term1) term2 = if(allContains (allVars term2) list) 
                                    then term1 
                                    else term2

compose :: Subst -> Subst -> Subst
compose sub1 sub2 = sub1                                    

without ::  [VarName] -> [VarName] -> [VarName] 
without list (r:rs) = if (contains r list) 
                            then without (delete r list) rs 
                            else without list rs

--  true if    this \/ is in this \/
allContains :: [VarName] -> [VarName] -> Bool
allCantains []  _ = True 
allContains (x:xs) list = if( contains x list ) then allContains xs list else False 