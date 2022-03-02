module Task5 where

import Type ( Term(..) )


ds :: Term -> Term -> Maybe (Term, Term)
ds (Var s1) (Comb s2 terms) = if ((Var s1) == (Comb s2 terms)) 
                                then fall3 (Var s1) (Comb s2 terms) 
                                else Just ((Var s1), (Comb s2 terms))
ds (Comb s1 terms) (Var s2) = if ((Comb s1 terms) == (Var s2))   
                                then fall3 (Comb s1 terms) (Var s2) 
                                else Just ((Comb s1 terms), (Var s2))    
ds term1 term2  = if ( term1 == term2) then Nothing else fall3 term1 term2
 where       
     fall3 :: Term -> Term -> Maybe (Term, Term)
     fall3 (Comb s1 (t1:t1s)) (Comb s2 (t2:t2s)) 
      | (s1 == s2) && (length (t1:t1s) == length (t2:t2s)) = if t1 /= t2 
                                                                then  Just (t1, t2) 
                                                                else fall3 (Comb s1 t1s) (Comb s2 t2s)
      | otherwise = Just ((Comb s1 (t1:t1s)), (Comb s2 (t2:t2s)))

