{-# LANGUAGE TemplateHaskell #-}
module Task5 where

import Task4 ( Subst, domain, empty, single, apply, compose )
import Type ( Term(Var, Comb),VarName (VarName))
import Task2 () 
import Test.QuickCheck ( quickCheckAll)
import Task3 (Vars(allVars))

ds :: Term -> Term -> Maybe (Term, Term)
ds (Var s1) (Comb s2 []) =  Just ((Var s1), (Comb s2 []))
ds (Comb s1 []) (Var s2) = Just ((Comb s1 []), (Var s2))
ds (Var s1) (Comb s2 terms) =  Just ((Var s1), (Comb s2 terms))
ds (Comb s1 terms) (Var s2) = Just ((Comb s1 terms), (Var s2))
ds term1 term2  = if (term1 == term2) 
                    then Nothing
                    else fall3 term1 term2
 where
     fall3 :: Term -> Term -> Maybe (Term, Term)
     fall3 (Comb s1 (t1:t1s)) (Comb s2 (t2:t2s)) 
      | (s1 == s2) && (length (t1:t1s) == length (t2:t2s)) = if t1 /= t2 
                                                                then ds t1 t2
                                                                else fall3 (Comb s1 t1s) (Comb s2 t2s)
      | otherwise = Just ((Comb s1 (t1:t1s)), (Comb s2 (t2:t2s)))
     fall3 t1 t2 = Just ( t1, t2) 


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
prop_test2 :: Term -> Term -> Bool
prop_test2 t1 t2 = if ds t1 t2 /= Nothing then t1 /= t2 else True
prop_test3 :: Term -> Term -> Bool
prop_test3 t1 t2 = if ds t1 t2 == Nothing then not (isNothing (unify t1 t2)) && domainMaybe (unify t1 t2) == [] else True
prop_test4 :: Term -> Term -> Bool
prop_test4 t1 t2 = if not (isNothing (unify t1 t2)) 
    then isNothing (ds (applyMaybe (unify t1 t2) t1) (applyMaybe (unify t1 t2) t2)) 
    else True

myTest :: Maybe Subst
myTest =unify (Comb "e" [(Comb "m" []),(Var (VarName "M"))]) (Comb "e" [(Var (VarName "F")), (Comb  "h" []) ])

myTest2 :: Maybe Subst
myTest2 = unify ( (Comb "equ" [( Comb "f" [(Comb "1" [])]) ,(Comb "g" [(Var (VarName "X"))]) ])) (Comb "equ" [(Var (VarName "Y")) ,(Var (VarName "Y")) ])

myTest3 :: Maybe (Term,Term)
myTest3 = ds (Var (VarName "X")) (Comb "f" [(Var (VarName "X"))])


myterm :: Term
myterm = Comb "g" [Var (VarName "_0"),Var (VarName "_0")]
myterm2 :: Term
myterm2 = Comb "g" [Var (VarName "B"),Comb "f" [Comb "f" [Var (VarName "_")]]]
myterm3 :: Term
myterm3 =  Comb "f" [ (Comb "g" [Comb "g" [Var (VarName "B")]]) , (Comb "f" [Var (VarName "_0")]) ]


isNothing :: Maybe a -> Bool 
isNothing (Just _) = False 
isNothing Nothing = True

domainMaybe :: Maybe Subst -> [VarName]
domainMaybe (Just subst) = domain subst 
domainMaybe _ = []


applyMaybe :: Maybe Subst -> Term -> Term 
applyMaybe (Just subst) term = apply subst term
applyMaybe _ term = term

return []
runTests :: IO Bool
runTests = $Test.QuickCheck.quickCheckAll