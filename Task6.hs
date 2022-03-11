{-# LANGUAGE TemplateHaskell #-}
module Task6  where 

import Type ( Rule(..), Term(..), VarName(..) )
import Task3 (Vars(allVars), freshVars)
import Task4 ( apply, single ) 
import Test.QuickCheck ( quickCheckAll, Property, (==>) ) 
import Data.List ( intersect )



rename :: [VarName] -> Rule -> Rule      
rename  notAllowed r = renameOhneAnons  (anonRename (notAllowed ++ (allVars r))  (getAllowedVars (notAllowed ++ allVars r) ) r) (filter (\x -> x /= (VarName "_")) (allVars r) )

-- erhält die Regel mit schon umbennanten anons und die nicht erlaubten sowie die noch umzubennenenden Vars 
-- dabei wird immer die erste noch umzubennennede Variable auf die erste erlaubte Var abgebildet,
-- wobei diese dann zu den nicht erlaubten hinzugefügt wird.
renameOhneAnons :: ( Rule, [VarName] ) -> [VarName] -> Rule
renameOhneAnons (r , _ ) [] = r
renameOhneAnons (r , notAllowed) (v:vs) = renameOhneAnons ( replaceAllWith v (head (getAllowedVars (notAllowed))) r   , (head (getAllowedVars (notAllowed))) : notAllowed ) vs

-- ersetzt alle ersten vars mit zweiten vars in der Regel
replaceAllWith :: VarName -> VarName -> Rule -> Rule
replaceAllWith v with (Rule t ts) = Rule ( apply (single v (Var with)) t  )  (map (\x -> apply (single v (Var with)) x ) ts )

-- gibt mithilfe von nicht erlaubten Vars die Erlaubten zurück
getAllowedVars :: [VarName] -> [VarName]
getAllowedVars notAllowdList = filter (\x -> notElem x (notAllowdList) ) freshVars 

-- aus nicht erlaubt, erlaubt und regel wird ein Tupel mit Regel ohne anons und die neue liste an nicht erlaubten Vars 
anonRename :: [VarName] -> [VarName] -> Rule -> (Rule, [VarName])
anonRename list (a:as) rule = if notElem (VarName "_") (allVars rule)
    then (rule , list)
    else anonRename (a : list) as (renameFirstAnonWith a rule)
anonRename _ [] _  = error "there is no way that an infinit list is empty"

-- macht die erste erscheinung von ner anon in ner Regel zu a 
renameFirstAnonWith :: VarName -> Rule -> Rule 
renameFirstAnonWith a (Rule t ts) | elem (VarName "_") (allVars t) =  Rule (renameFirstVar a t) ts
                                  | otherwise =  Rule t (renameFirstTerm a ts)

-- macht die erste erscheinung von anont in ner TermListe  zu a
renameFirstTerm :: VarName -> [Term] -> [Term]
renameFirstTerm _ [] = error "hier sollte eine anonyme var sein"
renameFirstTerm a (t:ts)  | elem (VarName "_") (allVars t) =  (renameFirstVar a t) : ts
                          | otherwise = t: (renameFirstTerm a ts)

-- macht die erste erscheinung von nem anon in nem term anon zu a
renameFirstVar :: VarName -> Term -> Term 
renameFirstVar a (Var s)  = if s == (VarName "_") then (Var a) else error "hier sollte eine anonyme var sein"
renameFirstVar a (Comb s terms) = Comb s (renameFirstTerm a terms)

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