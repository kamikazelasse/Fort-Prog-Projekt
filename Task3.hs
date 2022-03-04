module Task3  where

import Type ( Goal(..), Prog(..), Rule(..), Term(..), VarName(..) ) 
import Data.Char ( ord, chr )

class Vars a where
      allVars :: a -> [VarName]

--instance to retrieve all vars from a term
instance Vars Term where
    allVars (Var varName) = [varName]
    allVars (Comb _ terms ) = doVarList [] terms -- fold
     where 
        -- get all vars using accumulator technic
        doVarList :: [VarName] -> [Term] -> [VarName]
        doVarList list [] = list
        doVarList list ( (Var v):ts) = if ( elem v list) 
                                          then doVarList list ts 
                                          else doVarList (v:list) ts
        doVarList list ((Comb _ list2):ts) = doVarList (doVarList list list2) ts

--instance to retrieve all vars from a rule
instance Vars Rule where
    allVars (Rule term []) = allVars term
    allVars (Rule term (x:xs)) = removeDuplikates(allVars term ++ (allVars (Rule x xs)))

--instance to retrieve all vars from a prog
instance Vars Prog where
    allVars (Prog []) = []
    allVars (Prog (x:xs)) = removeDuplikates(allVars x ++ (allVars (Prog xs)))

--instance to retrieve all vars from a goal
instance Vars Goal where
    allVars (Goal []) = []
    allVars (Goal (x:xs)) = removeDuplikates(allVars x ++ (allVars (Goal xs)))

-- infinite list of variabels 
freshVars :: [VarName]
freshVars = [VarName (get n)| n <- [0..]]
 where 
     -- converts a number to a char and number using the given rules
     get :: Int -> String
     get n = [chr((mod n 26) + ord 'A')] ++ if((div (n - 26) 26) < 0) then "" else show (div (n - 26) 26)

removeDuplikates :: [VarName] -> [VarName] 
removeDuplikates list = removeDuplikatesRec [] list
 where 
     removeDuplikatesRec inList [] = inList 
     removeDuplikatesRec inList (x:xs) = if (elem x inList) 
                                           then removeDuplikatesRec inList xs 
                                           else removeDuplikatesRec (x:inList) xs