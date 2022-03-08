{-# LANGUAGE TemplateHaskell #-}
module Task6 (rename) where 

import Type ( Rule(..), Term(..), VarName(..) )
import Task3 (Vars(allVars), freshVars)
import Task4 (Subst, empty, compose, single, apply, domain)
import Test.QuickCheck ( quickCheckAll, Property, (==>) ) 
import Data.List ( intersect )


rename :: [VarName] -> Rule -> Rule
rename notAllowdList toRename = rename2 notAllowdList (renameAnons toRename ((allVars toRename) ++ notAllowdList))

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

renameAnons :: Rule -> [VarName] -> Rule
renameAnons (Rule t ts) list = if  elem (VarName "_") (allVars (Rule t ts)) 
 then Rule (head (renameAnons2 [t] 0)) (renameAnons2 (ts) (countAnons t))
 else (Rule t ts)
 where
    renameAnons2 :: [Term] -> Int -> [Term]
    renameAnons2 []  _ =  []
    renameAnons2 ((Var (VarName "_")):terms) n = if elem (freshVars !! n) list 
        then renameAnons2 ((Var (VarName "_")):terms) (n+1)
        else (Var (freshVars !! n)) : renameAnons2 terms (n+1)
    renameAnons2 ((Var (VarName s)):terms) n = (Var (VarName s)) : renameAnons2 ts n

    renameAnons2 ((Comb s innerterms): terms)  n = if elem  (VarName "_") (allVars (Comb s innerterms) )
        then (Comb s (renameAnons2 innerterms n) ) : renameAnons2 terms (n + countAnons (Comb s innerterms))
        else (Comb s innerterms):  renameAnons2 terms n


countAnons :: Term -> Int 
countAnons (Var( VarName s)) = if s == "_" then 1 else 0
countAnons (Comb _ terms) = foldr(\x r-> r + countAnons x ) 0 terms

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