module Task5 where

import Task4 (Subst (Subst), single)
import Type ( Term(..) )
import Distribution.Simple.InstallDirs (substPathTemplate)


ds :: Term -> Term -> Maybe (Term, Term)
ds (Var s1) (Comb s2 terms) =  Just ((Var s1), (Comb s2 terms))
ds (Comb s1 terms) (Var s2) = Just ((Comb s1 terms), (Var s2))  
ds term1 term2  = if ( term1 == term2) 
                    then Nothing
                    else fall3 term1 term2
 where       
     fall3 :: Term -> Term -> Maybe (Term, Term)
     fall3 (Comb s1 (t1:t1s)) (Comb s2 (t2:t2s)) 
      | (s1 == s2) && (length (t1:t1s) == length (t2:t2s)) = if t1 /= t2 
                                                                then  Just (t1, t2) 
                                                                else fall3 (Comb s1 t1s) (Comb s2 t2s)
      | otherwise = Just ((Comb s1 (t1:t1s)), (Comb s2 (t2:t2s)))
     fall3 t1 t2 = Just(t1, t2)

unify :: Term -> Term -> Maybe Subst
unify (Var n1) term2 = if(ds (Var n1) term2 == Nothing) then Nothing else Just (single n1 term2)
unify term1 (Var n2) = if(ds term1 (Var n2) == Nothing) then Nothing else Just (single n2 term1)
unify (Comb n1 terms) term2 = if(ds (Comb n1 terms) term2) == Nothing then Nothing else Just()
 
unify _ _ = Nothing