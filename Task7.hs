module Task7 where

import Task4 ( apply, empty, Subst (Subst) )
import Type ( Goal(..), Prog(..), Rule(..), Term (Var, Comb), VarName(VarName) )
import Task5 ( unify, isNothing )
import Task3 ( Vars(allVars) )
import Task6 ( rename )
import Task2 ( Pretty(..) )

data SLDTree = SLDTree Goal [(Subst,SLDTree)] 
 deriving Show

sldLeaf :: Goal -> SLDTree
sldLeaf goal = SLDTree goal []

sld :: Prog -> Goal -> SLDTree
sld prog goal = sldOhne prog goal ( allVars goal)
 
sldOhne :: Prog -> Goal -> [VarName] -> SLDTree
sldOhne (Prog []) goal _ = sldLeaf goal
sldOhne (Prog rules) (Goal tems) ohne = SLDTree (Goal tems) (unifyRules (map (\r -> rename ohne r)  rules) tems  0)



unifyRules ::  [Rule] -> [Term] -> Int -> [(Subst, SLDTree)]
unifyRules  _ [] _  = []
unifyRules  rules (t:ts) n = if n >= length rules
    then [] 
    else if canApplyRule (rules !! n) t
            then  [(getSubst rules t , sld (Prog rules)  (Goal ((applyRule (rules !! n) t) ++ ts)))] ++ unifyRules (map (\r -> rename []r) rules)  (t:ts) (n+1)
            else  unifyRules rules (t:ts)  (n+1)


getSubst :: [Rule] -> Term -> Subst 
getSubst  [] t = empty 
getSubst  ((Rule term res): rules) t = if isNothing (unify term t) 
    then getSubst rules t
    else getJust (unify term t)
    where
        getJust :: Maybe Subst -> Subst
        getJust (Just subst) = subst
        getJust Nothing = empty 

applyRule :: Rule -> Term -> [Term] 
applyRule (Rule term res) t = if isNothing (unify term t) 
    then error (" error ")
    else res 

canApplyRule :: Rule -> Term -> Bool 
canApplyRule (Rule term res) t = not (isNothing (unify term t))

instance Pretty SLDTree where
    pretty tree = make tree 0
     where
         make :: SLDTree ->Int -> String
         make (SLDTree goal []) n = repeat "|   " n ++ pretty goal ++ "\n"
         make (SLDTree goal (branch)) n = repeat "|   " n ++ pretty goal ++ "\n" ++ makeBranch branch n

         makeBranch :: [(Subst , SLDTree)] -> Int -> String
         makeBranch [] n = ""
         makeBranch ((subst,tree):other ) n = repeat "|   " n ++ "+-- " ++ pretty subst ++ "\n" ++ make tree (n+1) ++ makeBranch other n

         repeat :: String -> Int -> String 
         repeat _ 0 = ""
         repeat s n = s++ repeat s (n-1)   


p :: Prog
p = Prog [Rule (Comb "append" [Var (VarName "[]"), Var (VarName "Ys"), Var (VarName "Ys")]) [], Rule (Comb "append" [Comb "." [Var (VarName "X"), Var (VarName "Xs")], Var (VarName "Ys"), Comb "." [Var (VarName "X"), Var (VarName "Zs")]]) [Comb "append" [Var (VarName "Xs"), Var (VarName "Ys"), Var (VarName "Zs")]]]

g :: Goal
g = Goal [Comb "append" [Var (VarName "X"), Var (VarName "Y"), Comb "." [Comb "1" [], Comb "[]" []]]]

g2 :: Goal
g2 = Goal [Comb "append" [Var (VarName "X"), Var (VarName "Y"), Var (VarName "Z")]]
