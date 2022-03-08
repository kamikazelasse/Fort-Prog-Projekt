{-# LANGUAGE TemplateHaskell #-}
module Task5 where

import Type ( Term(Var, Comb),VarName (VarName), CombName)
import Task2 () 
import Task4 ( Subst, domain, empty, single, apply, compose )
import Test.QuickCheck ( quickCheckAll, Property, (==>))
import Task3 (Vars(allVars))
import Data.Maybe ( isNothing )


ds :: Term -> Term -> Maybe (Term, Term) 
ds (Var (VarName "_")) (Comb s2 terms) = Nothing
ds (Comb s1 terms) (Var (VarName "_")) = Nothing
ds (Var s1) (Comb s2 terms) = Just ((Var s1), (Comb s2 terms))
ds (Comb s1 terms) (Var s2) = Just ((Comb s1 terms), (Var s2))
ds term1 term2  = if term1 == term2 then Nothing else fall3 term1 term2
 where
     fall3 :: Term  -> Term  -> Maybe (Term, Term)
     fall3 (Comb s1 []) (Comb s2 []) = Nothing 
     fall3 (Comb s1 (t1:t1s)) (Comb s2 (t2:t2s)) 
      | (s1 == s2) && (length (t1:t1s) == length (t2:t2s)) = if t1 /= t2 
                                                                then if t1 == Var (VarName "_" ) || t2 == Var (VarName "_" ) then fall3 (Comb s1 t1s) (Comb s2 t2s) else ds t1 t2
                                                                else fall3 (Comb s1 t1s) (Comb s2 t2s)
      | otherwise = Just ((Comb s1 (t1:t1s)), (Comb s2 (t2:t2s)))
     fall3 t1 t2 = Just (t1, t2)


unify :: Term -> Term -> Maybe Subst
unify t1 t2 = if isNothing (ds t1 t2)   -- 2.
                then Just empty 
                else if ( isDrittends (ds t1 t2) ) 
                       then composeMaybe (unify (apply (getSingle (ds t1 t2)) t1 ) (apply (getSingle (ds t1 t2)) t2)) (getSingle (ds t1 t2))  
                       else Nothing -- sonst Fail
 where 
     getSingle :: Maybe (Term, Term) -> Subst
     getSingle (Just ((Var s) , term2)) = single s term2
     getSingle (Just (term1 , (Var s))) = single s term1
     getSingle _ = error ("has to be at least one var")

     composeMaybe ::  Maybe Subst -> Subst -> Maybe Subst 
     composeMaybe Nothing _  = Nothing
     composeMaybe (Just subst1) subst2  = Just (compose subst1 subst2)

     isDrittends :: Maybe (Term , Term) -> Bool 
     isDrittends (Just ((Var s), term)) = not (elem s (allVars term))
     isDrittends (Just (term, (Var s))) = not (elem s (allVars term))
     isDrittends _ = False     


--------------------------------- Automatic Tests -----------------------------------------------

prop_test1 :: Term -> Bool
prop_test1 t = ds t t == Nothing
prop_test2 :: Term -> Term -> Property 
prop_test2 t1 t2 = ds t1 t2 /= Nothing ==> t1 /= t2
prop_test3 :: Term -> Term -> Bool 
--ds t1 t2 == Nothing ==> not (isNothing (unify t1 t2)) && domainMaybe (unify t1 t2) == [] quickCheck gives up bcs it's unlikely 
-- to generate tests where ds t1 t2 == Nothing is true
prop_test3 t1 t2 = if (ds t1 t2) == Nothing  then (not (isNothing(unify t1 t2)) && domainMaybe (unify t1 t2) == []) else True
prop_test4 :: Term -> Term -> Property 
prop_test4 t1 t2 = not (isNothing (unify t1 t2)) ==> isNothing (ds (applyMaybe (unify t1 t2) t1) (applyMaybe (unify t1 t2) t2))


domainMaybe :: Maybe Subst -> [VarName]
domainMaybe (Just subst) = domain subst 
domainMaybe _ = []

applyMaybe :: Maybe Subst -> Term -> Term 
applyMaybe (Just subst) term = apply subst term
applyMaybe _ term = term

return []
runTests :: IO Bool
runTests = $Test.QuickCheck.quickCheckAll