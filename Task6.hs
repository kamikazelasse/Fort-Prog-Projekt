{-# LANGUAGE TemplateHaskell #-}
module Task6 where

import Type ( Rule(..), Term(..), VarName(..) )
import Task3 (Vars(allVars), freshVars)
import Task4 (Subst, empty, compose, single, apply, domain)
import Test.QuickCheck ( quickCheckAll, Property, (==>) ) 
import Data.List ( intersect )


rename :: [VarName] -> Rule -> Rule
rename notAllowdList toRename = rename2 notAllowdList (renameAllAnons toRename ((allVars toRename) ++ notAllowdList))

rename2 :: [VarName] -> Rule -> Rule
rename2 notAllowdList2 toRename2 = actualRename (createSubst (notAllowdList2 ++ (allVars toRename2)) empty 0 ) toRename2

createSubst :: [VarName] -> Subst -> Int -> Subst
createSubst [] sub _ = sub
createSubst (l:list) sub n = if (elem (freshVars !! n) (l:list)) || elem (freshVars !! n) (domain sub)
    then createSubst (l:list) sub (n+1)
    else createSubst list (compose (single l (Var (freshVars !! n))) sub) (n+1)

actualRename :: Subst -> Rule  -> Rule
actualRename s (Rule t ts)  = Rule (apply s t ) (map (\term -> (apply s term)) ts)

---------------------------------- rename all anonymis vars in the Rule -----------------------------------------

renameAllAnons ::Rule -> [VarName] -> Rule
renameAllAnons r list = if elem (VarName "_") (allVars r)
    then renameAllAnons (renameFirstAnonyms r list 0) list
    else r

renameFirstAnonyms :: Rule -> [VarName] -> Int -> Rule
renameFirstAnonyms (Rule t ts) varNameList n = if (elem (freshVars !! n) varNameList) 
    then renameFirstAnonyms (Rule t ts) varNameList (n+1)
    else if elem (VarName "_") (allVars t) 
        then Rule (renameTerm (freshVars !! n) t) ts 
        else Rule t (renameTermList (freshVars !! n) ts)

renameTerm :: VarName -> Term -> Term
renameTerm x (Var _ ) = Var x 
renameTerm _ (Comb _ []) = error ("in this Term should be a Var(Varname '_')")
renameTerm x (Comb s terms) = (Comb s (renameTermList x terms))

renameTermList :: VarName -> [Term] -> [Term]
renameTermList _ [] =  error ("in this TermList should be a Var(Varname '_')")
renameTermList x (t:ts) = if ( elem (VarName "_") (allVars t))
    then (renameTerm x t) : ts
    else t : renameTermList x ts

myTerm1 :: [VarName]
myTerm1 = [VarName "B"]
myTerm2 :: Rule
myTerm2 = Rule (Var (VarName "_0")) [Comb "f" []]
-----------------------------------------------------------------------------------------------------------------


prop_test1 ::  [VarName] -> Rule -> Bool 
prop_test1 xs r = intersect(allVars (rename xs r)) (allVars r) == []
prop_test2 :: [VarName] -> Rule -> Bool 
prop_test2 xs r = intersect(allVars (rename xs r)) xs == [] 
prop_test3 ::  [VarName] -> Rule -> Bool
prop_test3 xs r = notElem (VarName "") (allVars (rename xs r))
prop_test4 ::  [VarName] -> Rule -> Property 
prop_test4 xs r = notElem (VarName"") (allVars r) ==> length (allVars (rename xs r)) == length (allVars r)
prop_test5 ::  [VarName] -> Rule -> Bool
prop_test5 xs r = length (allVars(rename xs r)) >= length (allVars r)

return []
runTests :: IO Bool
runTests = $quickCheckAll