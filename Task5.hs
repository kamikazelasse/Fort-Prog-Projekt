{-# LANGUAGE TemplateHaskell #-}
module Task5 where

import Type ( Term(Var, Comb),VarName (VarName), CombName)
import Task2 () 
import Task4 ( Subst, domain, empty, single, apply, compose )
import Test.QuickCheck ( quickCheckAll, Property, (==>))
import Task3 (Vars(allVars))
import Data.Maybe ( isNothing )


ds :: Term -> Term -> Maybe (Term, Term)
ds term1 term2 = case (term1,term2) of 
    ( Var (VarName "_") , _ ) -> Nothing -- wenn irgendwas und anonyme Var dann gibt es kein dissagremant 
    ( _ , Var (VarName "_") ) -> Nothing
    ( Var s1 , term2 )        -> if (Var s1) == term2 then Nothing else Just ((Var s1),term2) 
    ( term1 , Var s2 )        -> if term1 == (Var s2) then Nothing else Just ( term1, (Var s2))
    otherwise                 -> fall3 term1 term2
 where
    fall3 :: Term  -> Term  -> Maybe (Term, Term)
    fall3 (Comb s1 t1) (Comb s2 t2) 
        | (s1 /= s2) || (length t1 /= length t2) = Just ((Comb s1 t1), (Comb s2 t2))
        | otherwise  = head (foldr ( \(x,y) r -> if isNothing (ds x y) then r else (ds x y) :r    ) [Nothing] (zip t1 t2) )
    fall3 t1 t2 = Just (t1, t2) -- hier sollte keine Var mehr ankommen 


    -- fall3 :: Term  -> Term  -> Maybe (Term, Term)
    -- fall3 (Comb s1 []) (Comb s2 []) = if s1 == s2 then Nothing else  Just ((Comb s1 []),(Comb s2 []))
    -- fall3 (Comb s1 (t1:t1s)) (Comb s2 (t2:t2s)) 
    --  | (s1 == s2) && (length (t1:t1s) == length (t2:t2s)) = if t1 /= t2 
    --         then if t1 == Var (VarName "_" ) || t2 == Var (VarName "_" ) then fall3 (Comb s1 t1s) (Comb s2 t2s) else ds t1 t2
    --         else fall3 (Comb s1 t1s) (Comb s2 t2s)
    --  | otherwise = Just ((Comb s1 (t1:t1s)), (Comb s2 (t2:t2s)))
    -- fall3 t1 t2 = Just (t1, t2) -- hier sollte keine Var mehr ankommen  


-- die bis jetzt erstellte substitution ist eine leere subst (empty )
unify :: Term -> Term -> Maybe Subst
unify t1 t2 = unifyAcc t1 t2 empty

-- nutzt einen accumulator um die bis jetzt erstellte substitution zu speichern
unifyAcc :: Term -> Term -> Subst -> Maybe Subst
unifyAcc t1 t2 subst  = case (ds t1 t2) of
            Nothing -> Just subst 
            Just ( Var s, term )   -> if notElem s (allVars term)
                then unifyAcc (apply (single s term) t1 ) (apply (single s term) t2 ) (compose (single s term) subst)
                else Nothing
            Just ( term , Var s )  -> if notElem s (allVars term)
                then unifyAcc (apply (single s term) t1 ) (apply (single s term) t2 ) (compose (single s term) subst)
                else Nothing
            otherwise -> Nothing



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