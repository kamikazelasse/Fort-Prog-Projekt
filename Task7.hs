module Task7 where

import Type ( Goal(..), Prog(..), Rule(..), Term (Var, Comb), VarName(VarName) )
import Task2 ( Pretty(..) )
import Task3 ( Vars(allVars) )
import Task4 ( apply, empty, Subst (Subst), compose )
import Task5 ( unify, isNothing )
import Task6 ( rename )

data SLDTree = SLDTree Goal [(Subst,SLDTree)] 
 deriving Show

sldLeaf :: Goal -> SLDTree
sldLeaf goal = SLDTree goal []

sld :: Prog -> Goal -> SLDTree
sld prog goal = sldOhne prog goal []


sldOhne :: Prog -> Goal -> [VarName] -> SLDTree
sldOhne (Prog []) goal _ = sldLeaf goal
sldOhne (Prog rules) (Goal terms) ohne = SLDTree (Goal terms) (unifyRules (map (\r -> rename ohne r)  rules) terms  0)


unifyRules ::  [Rule] -> [Term] -> Int -> [(Subst, SLDTree)]
unifyRules  _ [] _  = []
unifyRules  rules (t:ts) n = if n >= length rules
    then [] 
    else if canApplyRule (rules !! n) t
            then  (getSubst (rules !! n) t , sldOhne (Prog rules)  (Goal ((applyRule (rules !! n) t (getSubst (rules !! n) t) ) ++ ts))  (allVars (Prog rules))) : unifyRules rules (t:ts) (n+1)
            else  unifyRules rules (t:ts)  (n+1)

getSubst :: Rule -> Term -> Subst 
getSubst  (Rule term res) t = getJust (unify term t)
    where
        getJust :: Maybe Subst -> Subst
        getJust (Just subst) = subst
        getJust Nothing = empty 

applyRule :: Rule -> Term -> Subst -> [Term] 
applyRule (Rule term res) t subst = if isNothing (unify term t) 
    then error (" error ")
    else map (\x -> apply subst x) res 

canApplyRule :: Rule -> Term -> Bool 
canApplyRule (Rule term res) t = not (isNothing (unify term t))

instance Pretty SLDTree where
    pretty tree = make tree 0
     where
         make :: SLDTree ->Int -> String
         make (SLDTree goal []) n = if n > 6 then repeat "|   " n ++ "...\n" else repeat "|   " n ++ pretty goal ++ "\n"
         make (SLDTree goal (branch)) n = if n > 6 then repeat "|   " n ++ "...\n" else repeat "|   " n ++ pretty goal ++ "\n" ++ makeBranch branch n

         makeBranch :: [(Subst , SLDTree)] -> Int -> String
         makeBranch [] n = ""
         makeBranch ((subst,tree):other ) n = repeat "|   " n ++ "+-- " ++ pretty subst ++ "\n" ++ make tree (n+1) ++ makeBranch other n

         repeat :: String -> Int -> String 
         repeat _ 0 = ""
         repeat s n = s++ repeat s (n-1)   


type Strategy = SLDTree -> [Subst]

dfs :: Strategy
dfs (SLDTree _ []) = []
dfs (SLDTree goal ((s,sldtree):branches)) = bilddfs sldtree s  ++ dfs (SLDTree goal branches)
 where
     bilddfs (SLDTree _ []) subst = [subst]
     bilddfs (SLDTree goal ((s,sldtree):branches)) subst = bilddfs sldtree (compose s subst) ++ dfs (SLDTree goal branches)

--map (\x  -> compose x s ) (dfs sldtree  ++ dfs (SLDTree goal branches))

bfs :: Strategy 
bfs(SLDTree goal []) = [] 
bfs (SLDTree goal branches) = []
 where 
     subsList []=[]
     subsList ((x,_): b) = x: subsList b 

     treeList []=[]
     treeList ((_,x): b) = x: treeList b 

     bildbfs :: [Subst] -> [SLDTree] -> [Subst]
     bildbfs [] _ = []
     bildbfs _ [] = []
     bildbfs (s:ss) (t:ts) = [] -- TODO !!


--( map (\x -> compose s x) (bfs (SLDTree goal branches) ++ bfs sldtree))

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g strat = strat (sld p g)

instance Pretty [Subst] where
    pretty [] = ""
    pretty (s:ss) = pretty s ++ "\n" ++ pretty ss


p :: Prog
p = Prog [Rule (Comb "append" [Var (VarName "[]"), Var (VarName "Ys"), Var (VarName "Ys")]) [], Rule (Comb "append" [Comb "." [Var (VarName "X"), Var (VarName "Xs")], Var (VarName "Ys"), Comb "." [Var (VarName "X"), Var (VarName "Zs")]]) [Comb "append" [Var (VarName "Xs"), Var (VarName "Ys"), Var (VarName "Zs")]]]

g :: Goal
g = Goal [Comb "append" [Var (VarName "X"), Var (VarName "Y"), Comb "." [Comb "1" [], Comb "[]" []]]]

g2 :: Goal
g2 = Goal [Comb "append" [Var (VarName "X"), Var (VarName "Y"), Var (VarName "Z")]]