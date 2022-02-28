import Type (VarName(VarName), CombName, Term(Var, Comb), Rule(Rule), Prog(Prog), Goal(Goal))
import Data.Char ( ord, chr )

class Vars a where
      allVars :: a -> [VarName]


instance Vars Term where
    allVars (Var varName) = [varName]
    allVars (Comb _ terms ) = doVarList [] terms
     where 
        doVarList :: [VarName] -> [Term] -> [VarName]
        doVarList list [] = list
        doVarList list ( (Var v):ts) = if ( contains v list) 
                                          then doVarList list ts 
                                          else doVarList (v:list) ts
        doVarList list ((Comb _ list2):ts) = doVarList (doVarList list list2) ts

instance Vars Rule where
    allVars (Rule term []) = allVars term
    allVars (Rule term (x:xs)) = removeDuplikates(allVars term ++ (allVars (Rule x xs)))

instance Vars Prog where
    allVars (Prog []) = []
    allVars (Prog (x:xs)) = removeDuplikates(allVars x ++ (allVars (Prog xs)))

instance Vars Goal where
    allVars (Goal []) = []
    allVars (Goal (x:xs)) = removeDuplikates(allVars x ++ (allVars (Goal xs)))


freshVars :: [VarName]
freshVars = [VarName (get n)| n <- [0..]]
 where 
     get :: Int -> String
     get n = [chr((mod n 26) + ord 'A')] ++ if((div (n - 26) 26) < 0) then "" else show (div (n - 26) 26)


contains :: VarName -> [VarName] -> Bool 
contains _ [] = False 
contains a (x:xs) = if( a == x ) then True else contains a xs


removeDuplikates :: [VarName] -> [VarName] 
removeDuplikates list = removeDuplikatesRec [] list
 where 
     removeDuplikatesRec inList [] = inList 
     removeDuplikatesRec inList (x:xs) = if (contains x inList) 
                                           then removeDuplikatesRec inList xs 
                                           else removeDuplikatesRec (x:inList) xs


-- Dennis hats gefixt