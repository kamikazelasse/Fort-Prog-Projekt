{-# LANGUAGE TemplateHaskell #-}
module Task6 (rename) where 

import Type ( Rule(..), Term(..), VarName(..) )
import Task3 (Vars(allVars), freshVars)
import Task4 (Subst, empty, compose, single, apply, domain)
import Test.QuickCheck ( quickCheckAll, Property, (==>) ) 
import Data.List ( intersect, nub )


rename :: [VarName] -> Rule -> Rule
rename notAllowdList toRename = rename2 notAllowdList (renameAnons toRename (nub ((allVars toRename) ++ notAllowdList)))

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
 then Rule (head (renameAnons2 [t] 0)) (tail (renameAnons2 (t:ts) 0))
 else (Rule t ts)
 where
    renameAnons2 :: [Term] -> Int -> [Term]
    renameAnons2 []  _ =  []
    renameAnons2 terms i = case terms of
                            ((Var (VarName "_")):rest)  ->  if elem (freshVars !! i) list 
                                                            then renameAnons2 terms (i+1) 
                                                            else (Var (freshVars !! i)) : renameAnons2 rest (i+1)
                            ((Var (VarName s)):rest)    ->  (Var(VarName s)) : renameAnons2 rest i
                            ((Comb s rest):rest2)       ->  (Comb s (renameAnons2 rest i)) : (renameAnons2 rest2 (i + actualCount rest))

countAnons :: Term -> Int 
countAnons (Var( VarName s)) = if s == "_" then 1 else 0
countAnons (Comb _ terms) = foldr(\x r-> r + countAnons x ) 0 terms

actualCount :: [Term] -> Int 
actualCount = foldr (\x r -> (countAnons x) + r ) 0

-----------------------------------------------------------------------------------------------------------------


prop_test1 ::  [VarName] -> Rule -> Bool 
prop_test1 xs r = intersect(allVars (rename xs r)) (allVars r) == []
prop_test2 :: [VarName] -> Rule -> Bool 
prop_test2 xs r = intersect(allVars (rename xs r)) xs == [] 
prop_test3 ::  [VarName] -> Rule -> Bool
prop_test3 xs r = notElem (VarName "_") (allVars (rename xs r))
prop_test4 ::  [VarName] -> Rule -> Property 
prop_test4 xs r = notElem (VarName"_") (allVars r) ==> length (allVars (rename xs r)) == length (allVars r)
prop_test5 ::  [VarName] -> Rule -> Bool
prop_test5 xs r = length (allVars(rename xs r)) >= length (allVars r)

return []
runTests :: IO Bool
runTests = $quickCheckAll