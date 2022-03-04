module Task3  where

import Type ( Goal(..), Prog(..), Rule(..), Term(..), VarName(..) ) 
import Data.Char ( ord, chr )
import Data.List (nub)

class Vars a where
      allVars :: a -> [VarName]

--instance to retrieve all vars from a term
instance Vars Term where
    allVars (Var varName) = [varName]
    allVars (Comb _ terms ) = allVars terms

--instance to retrieve all vars from a rule
instance Vars Rule where
    allVars (Rule t ts) = allVars (t:ts)

--instance to retrieve all vars from a prog
instance Vars Prog where
    allVars (Prog rs) = allVars rs

--instance to retrieve all vars from a goal
instance Vars Goal where
    allVars (Goal goals) = allVars goals

--instance to retrieve all vars from a List of things where allVars is defined
instance (Vars a) => Vars [a] where
    allVars list = nub (concat (map allVars (list)))

-- infinite list of variabels 
freshVars :: [VarName]
freshVars = [VarName (get n)| n <- [0..]]
 where 
     -- converts a number to a char and number using the given rules
     get :: Int -> String
     get n = [chr((mod n 26) + ord 'A')] ++ if((div (n - 26) 26) < 0) then "" else show (div (n - 26) 26)
