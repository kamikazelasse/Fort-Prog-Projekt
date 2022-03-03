{-# LANGUAGE TemplateHaskell #-}
import Type
import Task3 (Vars(allVars), contains, freshVars)
import Task41 (Subst, empty, compose, single, apply, prop_test1)
import Test.QuickCheck ( quickCheckAll ) 




rename :: [VarName] -> Rule -> Rule
rename notAllowdList toRename = rename2 notAllowdList (renameAllAnons toRename (allVars toRename))
 where 
     rename2 :: [VarName] -> Rule -> Rule
     rename2 notAllowdList2 toRename2 = actualRename (createSubst (notAllowdList2 ++ (allVars toRename2)) empty 0 ) toRename2

     createSubst :: [VarName] -> Subst -> Int -> Subst
     createSubst [] sub _ = sub
     createSubst (l:list) sub n = if (contains (freshVars !! n) list)
         then createSubst (l:list) sub (n+1)
         else createSubst list (compose (single l (Var (freshVars !! n))) sub) (n+1)

     actualRename :: Subst -> Rule  -> Rule
     actualRename s (Rule t ts)  = Rule (apply s t ) (map (\term -> (apply s term)) ts)

---------------------------------- rename all anonymis vars in the Rule -----------------------------------------

renameAllAnons ::Rule -> [VarName] -> Rule
renameAllAnons r list = if contains (VarName "_") (allVars r)
    then renameAllAnons (renameFirstAnonyms r list 0) list
    else r

renameFirstAnonyms :: Rule -> [VarName] -> Int -> Rule
renameFirstAnonyms (Rule t ts) varNameList n = if (contains (freshVars !! n) varNameList) 
    then renameFirstAnonyms (Rule t ts) varNameList (n+1)
    else if contains (VarName "_") (allVars t) 
        then Rule (renameTerm (freshVars !! n) t) ts 
        else Rule t (renameTermList (freshVars !! n) ts)

renameTerm :: VarName -> Term -> Term
renameTerm x (Var _ ) = Var x 
renameTerm _ (Comb _ []) = error ("in this Term should be a Var(Varname '_')")
renameTerm x (Comb s terms) = (Comb s (renameTermList x terms))

renameTermList :: VarName -> [Term] -> [Term]
renameTermList _ [] =  error ("in this TermList should be a Var(Varname '_')")
renameTermList x (t:ts) = if ( contains (VarName "_") (allVars t))
    then (renameTerm x t) : ts
    else t : renameTermList x ts

-----------------------------------------------------------------------------------------------------------------


prop_test1 ::  [VarName] -> Rule -> Bool 
prop_test1  xs r = (allVars (rename xs r)) 

return []
runTests :: IO Bool
runTests = $Test.QuickCheck.quickCheckAll