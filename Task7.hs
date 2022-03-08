module Task7 (sld , dfs ,bfs , solveWith, Strategy) where

import Type ( Goal(..), Prog(..), Rule(..), Term(..), VarName(..) ) 
import Task2 ( Pretty(..) )
import Task3 ( Vars(allVars) )
import Task4 ( apply, compose, empty, restrictTo, Subst ) 
import Task5 ( unify )
import Task6 (rename)
import Data.Maybe ( isNothing, isJust, fromJust )
import Distribution.Simple.Command (OptDescr(BoolOpt))

data SLDTree = SLDTree Goal [(Subst,SLDTree)] 
 deriving Show

sldLeaf :: Goal -> SLDTree
sldLeaf goal = SLDTree goal []

isLeaf :: SLDTree -> Bool 
isLeaf (SLDTree _ []) = True 
isLeaf _ = False

sld :: Prog -> Goal -> SLDTree
sld prog goal = sldOhne prog goal []


sldOhne :: Prog -> Goal -> [VarName] -> SLDTree
sldOhne (Prog []) goal _ = sldLeaf goal
sldOhne (Prog rules) (Goal terms) ohne = SLDTree (Goal terms) (tryRules (map (\r -> rename ohne r)  rules) terms  0)


tryRules ::  [Rule] -> [Term] -> Int -> [(Subst, SLDTree)]
tryRules  _ [] _  = []
tryRules  rules (t:ts) n = if n >= length rules
    then [] 
    else if canApplyRule (rules !! n) t -------------------------- Besser ---------------------------------
            then  (getSubst (rules !! n) t , sldOhne (Prog rules) (Goal (map (\x -> apply (getSubst (rules !! n) t) x) (applyRule (rules !! n) ++ ts)))  (allVars (Prog rules))) : tryRules rules (t:ts) (n+1)
            else  tryRules rules (t:ts)  (n+1)

getSubst :: Rule -> Term -> Subst 
getSubst  (Rule term _) t = fromJust (unify t term)

applyRule :: Rule -> [Term] 
applyRule (Rule _ res) = res

canApplyRule :: Rule -> Term -> Bool 
canApplyRule (Rule term res) t = isJust (unify term t)

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
dfs (SLDTree (Goal []) _) = [empty] ----------------------------- Neu ------------------------------------------
dfs (SLDTree _ []) = []
dfs (SLDTree goal ((s,sldtree):branches)) = map (\x -> compose x s) (dfs sldtree) ++ dfs (SLDTree goal branches)



bfs :: Strategy
bfs (SLDTree _ branches) = bfsR branches ----------------------------- TODO ! ------------------------------------------
 where 
     bfsR :: [(Subst, SLDTree)] -> [Subst]
     bfsR [] = []
     bfsR list = foldr (\x r -> if isFin(snd x) then (fst x) : r else r) [] (oneStep list) ++ bfsR (filter (\x ->  not (isLeaf (snd x)) ) (oneStep list))

     isFin :: SLDTree -> Bool
     isFin (SLDTree (Goal []) _) = True
     isFin _ = False

     oneStep :: [(Subst, SLDTree)] -> [(Subst , SLDTree)] 
     oneStep list = foldr (\x r -> docompose (fst x) (snd x) ++ r) [] list

     docompose :: Subst -> SLDTree -> [(Subst , SLDTree)] 
     docompose s (SLDTree g []) = [(s , sldLeaf g)]
     docompose s (SLDTree _ list) = map (\l -> (compose (fst l) s, snd l)) list 



solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g strat = map (\x -> restrictTo x (allVars g) ) (strat (sld p g))
                     

instance Pretty [Subst] where -- nicht unnötig
    pretty [] = ""
    pretty (s:ss) = pretty s ++ "\n" ++ pretty ss
