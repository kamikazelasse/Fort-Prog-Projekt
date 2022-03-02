module Task5 where

import Type ( Term(..) )


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