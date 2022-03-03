module Task7 where
import Task41 ( apply, empty, Subst (Subst) )
import Type ( Goal(..), Prog(..), Rule(..), Term, VarName )
import Task5 ( unify, isNothing )
import Task3 ( Vars(allVars) )

data SLDTree = SLDTree Goal [(Subst,SLDTree)] 

sldLeaf :: Goal -> SLDTree
sldLeaf goal = SLDTree goal []

sld :: Prog -> Goal -> SLDTree
sld prog goal = sldOhne prog goal []
 where 
  sldOhne :: Prog -> Goal -> [VarName] -> SLDTree
  sldOhne (Prog []) goal _ = sldLeaf goal
  sldOhne (Prog rules) (Goal tems) ohne = SLDTree goal  (unifyRules rules tems (ohne ++ (allVars (Prog rules)) ++ (allVars (Goal tems))))



unifyRules ::  [Rule] -> [Term] -> [VarName] -> [(Subst, SLDTree)]
unifyRules  _ [] _ = []
unifyRules  rules (t:ts) list = if canApplyRules rules t
    then  error (" nicht lösbar ? ")
    else  [(getSubst rules t , sld (Prog rules)  (Goal ((applyRule rules t) ++ (map (\x -> apply (getSubst rules t) x) (ts)))))] ++ unifyRules rules (t:ts) list


getSubst :: [Rule] -> Term -> Subst 
getSubst  [] t = empty 
getSubst  ((Rule term res): rules) t = if isNothing (unify term t) 
    then getSubst rules t
    else getJust (unify term t)
    where
        getJust :: Maybe Subst -> Subst
        getJust (Just subst) = subst
        getJust Nothing = empty 

applyRule :: [Rule] -> Term -> [Term] 
applyRule [] t = [t]
applyRule ((Rule term res): rules) t = if isNothing (unify term t) 
    then applyRule rules t
    else res 

canApplyRules :: [Rule] -> Term -> Bool 
canApplyRules [] _ = False
canApplyRules ((Rule term res): rules) t = if isNothing (unify term t) 
    then canApplyRules rules t
    else True
