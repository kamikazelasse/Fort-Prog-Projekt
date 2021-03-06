{-# LANGUAGE TemplateHaskell #-}
module Task4 where

import Type ( Term(..), VarName ) 
import Task2 ( Pretty(..))
import Task3 ( Vars(..) ) 
import Test.QuickCheck (Arbitrary (arbitrary), quickCheckAll, Property, (==>))
import Data.List (nub)
import Data.Bits ( Bits(xor) )

data Subst = Subst [(VarName, Term)] 
 deriving Show

-- returns all variables not showing on them self
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

-- applies a substitution on a term and returns a term
apply :: Subst -> Term -> Term
apply (Subst [] ) term2 = term2
apply (Subst ((v,t):subs1)) (Var x) = if (v == x) then t else apply (Subst subs1) (Var x)
apply subst (Comb s terms) = Comb s (map (\x -> apply subst x) terms)

-- composes two subst to one
compose :: Subst -> Subst -> Subst
compose  (Subst []) subst2 = subst2
compose  subst1  (Subst []) = subst1
compose (Subst sub1) (Subst sub2) = Subst ((map (\(v,t) -> (v, (apply (Subst sub1) t))) sub2 ) ++ filter (\(v,_) -> isNotAKey v (Subst sub2 ))  sub1 )
 where 
        -- not neccessary but nicer
        isNotAKey :: VarName -> Subst -> Bool 
        isNotAKey v subs = not (elem v (domain subs))

-- restricts the subst to the given keys (VarName)
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst []) _ = empty
restrictTo (Subst subs) var = Subst (filter (\(x,_) -> elem x var) subs) 

-- makes subst pretty
instance Pretty Subst where 
    pretty (Subst []) = "{}"
    pretty (Subst subs) = "{" ++ foldr (\x r -> if xor ((prettyTupel x) /= "") (r /= "") then r ++ prettyTupel x else prettyTupel x ++ ", " ++ r)  "" subs ++ "}" 
     where 
         prettyTupel :: (VarName , Term ) -> String
         prettyTupel (v,t) =  ( if(Var v == t  ) then "" else pretty (Var v) ++ " -> " ++ pretty t)


-- instance to get all vars of a subst
instance Vars Subst where
    allVars subs = nub (domain subs ++ allVars (getTerms subs))

-- instance to recieve arbitrary substs
instance Test.QuickCheck.Arbitrary Subst where
 arbitrary = do 
                vars <- Test.QuickCheck.arbitrary  
                terms <- Test.QuickCheck.arbitrary 
                return (Subst (zip (nub vars) terms))


instance Pretty [Subst] where 
    pretty [] = ""
    pretty (s:ss) = pretty s ++ "\n" ++ pretty ss


------------------------------- Helperfunctions ---------------------------------------------

-- returns all terms of a subst
getTerms :: Subst -> [Term]
getTerms (Subst []) = []
getTerms (Subst ((_ , t) : subs)) = (t: getTerms (Subst subs))

--  true if      this  is in  this
allcontains :: [VarName] -> [VarName] -> Bool
allcontains []  _ = True
allcontains (x:xs) list = if( elem x list ) then allcontains xs list else False 

-- method to reduce code in tests 
--checks if two lists have the same set
setEq :: [VarName] -> [VarName] -> Bool 
setEq a b = allcontains a b && allcontains b a

--------------------------------- Automatic Tests -----------------------------------------------

prop_test1 :: Term -> Bool
prop_test1 t = apply empty t == t
prop_test2 :: VarName -> Term -> Bool 
prop_test2 x t = apply (single x t) (Var x) == t
prop_test3 :: Term -> Subst -> Subst -> Bool
prop_test3 t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)
prop_test4 :: Bool
prop_test4 = domain empty == []
prop_test5 :: VarName -> Bool
prop_test5 x = domain (single x (Var x)) == []
prop_test6 :: VarName -> Term -> Property
prop_test6 x t = t /= (Var x) ==> domain (single x t) == [x]
prop_test7 :: Subst -> Subst -> Bool
prop_test7 s1 s2 = allcontains (domain (compose s1 s2)) (domain s1 ++ domain s2)
prop_test8 :: VarName -> VarName -> Property 
prop_test8 x1 x2 = x1 /= x2 ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2]
prop_test9 :: Bool
prop_test9 = allVars empty == []
prop_test10 ::VarName -> Bool
prop_test10 x = allVars (single x (Var x)) == []
prop_test11 :: VarName -> Term -> Property 
prop_test11 x t = t /= (Var x) ==> setEq (allVars (single x t)) ([x] ++ (allVars t)) 
prop_test12 :: Subst -> Subst -> Bool
prop_test12 s1 s2 = allcontains(allVars (compose s1 s2)) (allVars s1 ++ allVars s2)
prop_test13 :: VarName -> VarName -> Property 
prop_test13 x1 x2 = x1 /= x2 ==> setEq (allVars (compose (single x2 (Var x1)) (single x1 (Var x2)))) [x1,x2]
prop_test14 :: Subst -> Bool
prop_test14 s = allcontains (domain s) (allVars s)
prop_test15 :: [VarName] -> Bool
prop_test15 xs = domain (restrictTo empty xs) == []
prop_test16 :: [VarName] -> Subst -> Bool
prop_test16 xs s = allcontains (domain(restrictTo s xs )) xs

return []
runTests :: IO Bool
runTests = $quickCheckAll