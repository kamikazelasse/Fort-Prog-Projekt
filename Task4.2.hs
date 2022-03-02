{-# LANGUAGE TemplateHaskell #-}
module Task4 where

import Type ( Goal(Goal), Term(Var, Comb), VarName(VarName)) 
import Task2 ( Pretty(..))
import Task3 ( Vars(..), contains, removeDuplikates ) 
import Test.QuickCheck (Arbitrary (arbitrary), quickCheckAll)

data Subst = Subst [(VarName, Term)] 
 deriving Show

domain :: Subst -> [VarName]
domain (Subst []) = []
domain (Subst ((v, (Var x)):subs)) = if( v == x ) 
                                      then domain (Subst subs) 
                                      else (v:domain (Subst subs))
domain (Subst ((v,_):subs)) = (v:domain (Subst subs))


empty :: Subst
empty = Subst []

single :: VarName -> Term -> Subst
single name (Var s) = if ( name == s ) then empty else Subst [(name, Var s)]
single name term = Subst [(name, term)]

apply :: Subst -> Term -> Term
apply (Subst [] ) term2 = term2
apply (Subst ((v , t ) : subs)) (Var x) = if(v == x) then t else apply (Subst subs) (Var x)
apply subst (Comb s terms) = Comb s (map (\x -> apply subst x) terms)


compose :: Subst -> Subst -> Subst
compose  subst1 (Subst subs) = add (createSubst (domain (Subst subs)) (map (\t -> apply subst1 t) (getTerms (Subst subs)) )) subst1 
 where 
        add :: Subst -> Subst -> Subst
        add subst2 (Subst []) = subst2
        add (Subst subs1) (Subst ((v,(Var n)): subs2)) =
            if ( contains v (domain (Subst subs1)))                   
                then add (Subst subs1) (Subst subs2) 
                else add (Subst (subs1 ++ [(v,(Var n))])) (Subst subs2)
        add (Subst subs1) (Subst ((v,t): subs2)) =
                 if ( contains v (domain (Subst subs1))) 
                  then add (Subst (without subs1 v)) (Subst subs2)
                  else add (Subst (subs1 ++ [(v,t)])) (Subst subs2)

       
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst []) _ = empty
restrictTo (Subst ((v,t): subs)) varlist = if(contains v varlist )
                                             then compose (Subst [(v,t)]) (restrictTo (Subst subs) varlist) 
                                             else (restrictTo (Subst subs) varlist)
       


instance Pretty Subst where 
    pretty (Subst []) = "{}"
    pretty subst = "{" ++ recPretty subst ++ "}" 
     where 
         recPretty :: Subst -> String
         recPretty (Subst []) = ""
         recPretty (Subst [(var,term)]) = if( [var] == allVars term) then "" else pretty (Var var) ++ " -> " ++  pretty term
         recPretty (Subst ((v,t): subs)) =  if( [v] == allVars t) 
                                                then recPretty (Subst subs) 
                                                else pretty (Var v) ++ " -> " ++ pretty t ++ ", " ++ recPretty (Subst subs)

instance Vars Subst where
    allVars subs = removeDuplikates (domain subs ++ allVars (Goal (getTerms subs)) )


instance Test.QuickCheck.Arbitrary Subst where
 arbitrary = do 
                vars <- Test.QuickCheck.arbitrary  
                terms <- Test.QuickCheck.arbitrary 
                return (createSubst vars terms )


------------------------------- Helperfunctions ---------------------------------------------
createSubst :: [VarName] -> [Term] -> Subst 
createSubst (v:vs) (t:ts) = compose (single v t) (createSubst vs ts)
createSubst [] _ = empty
createSubst _ [] = empty

getTerms :: Subst -> [Term]
getTerms (Subst []) = []
getTerms (Subst ((_ , t) : subs)) = (t: getTerms (Subst subs))

--           this without this   
without ::  [(VarName, Term)] -> VarName -> [(VarName, Term)] 
without [] _ = []
without ((v,t):subs)  v2 = if v == v2 then (subs) else ((v,t): without subs v2)

--  true if      this  is in  this 
allContains :: [VarName] -> [VarName] -> Bool
allContains []  _ = True
allContains (x:xs) list = if( contains x list ) then allContains xs list else False 

setEq :: [VarName] -> [VarName] -> Bool 
setEq a b = allContains a b && allContains b a


myterm :: Term
myterm = Comb "g" [Var (VarName "_")]
mysubs1 :: Subst
mysubs1 = Subst [(VarName "B",Comb "f" [Comb "f" [Var (VarName "_0"),Comb "f" []],Comb "f" []]),(VarName "_",Comb "f" []),(VarName "B",Comb "f" [Comb "f" [],Comb "g" [Var (VarName "_0"),Comb "g" [Var (VarName "_"),Comb "g" [Var (VarName "B")]]]]),(VarName "_",Var (VarName "_0")),(VarName "B",Var (VarName "_"))]
mysubs2 :: Subst
mysubs2 = Subst [(VarName "A",Comb "g" [Comb "g" [Var (VarName "_0"),Comb "g" []]]),(VarName "B",Comb "g" [Comb "g" [Var (VarName "B")],Var (VarName "A")]),(VarName "_",Var (VarName "_0")),(VarName "_",Comb "g" [Var (VarName "B")]),(VarName "B",Var (VarName "B"))]


--------------------------------- Automatic Tests -----------------------------------------------

prop_test1 :: Term -> Bool
prop_test1 t = apply empty t == t
prop_test2 :: VarName -> Term -> Bool 
prop_test2 x t =  apply(single x t) (Var x) == t
prop_test3 :: Term -> Subst -> Subst -> Bool
prop_test3 t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)
prop_test4 :: Bool
prop_test4 = domain empty == []
prop_test5 :: VarName -> Bool
prop_test5 x = domain (single x (Var x)) == []
prop_test6 :: VarName -> Term -> Bool
prop_test6 x t = if(t /= (Var x)) then domain (single x t) == [x]
                                  else True
prop_test7 :: Subst -> Subst -> Bool
prop_test7 s1 s2 = allContains (domain (compose s1 s2)) (domain s1 ++ domain s2)
prop_test8 :: VarName -> VarName -> Bool
prop_test8 x1 x2 = if(x1 /= x2) then domain (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2]
                                else True 
prop_test9 :: Bool
prop_test9 = allVars empty == []
prop_test10 ::VarName -> Bool
prop_test10 x = allVars (single x (Var x)) == []
prop_test11 :: VarName -> Term -> Bool 
prop_test11 x t = if(t /= (Var x)) then setEq (allVars (single x t)) ([x] ++ (allVars t)) 
                                   else True
prop_test12 :: Subst -> Subst -> Bool
prop_test12 s1 s2 = allContains(allVars (compose s1 s2)) (allVars s1 ++ allVars s2)
prop_test13 :: VarName -> VarName -> Bool
prop_test13 x1 x2 = if(x1 /= x2) then setEq (allVars (compose (single x2 (Var x1)) (single x1 (Var x2)))) [x1,x2] 
                                 else True
prop_test14 :: Subst -> Bool
prop_test14 s = allContains (domain s) (allVars s)
prop_test15 :: [VarName] -> Bool
prop_test15 xs = domain (restrictTo empty xs) == []
prop_test16 :: [VarName] -> Subst -> Bool
prop_test16 xs s = allContains (domain(restrictTo s xs )) xs
return []

runTests :: IO Bool
runTests = $quickCheckAll