module Task2 where

import Type (VarName(VarName), CombName, Term(Var, Comb), Rule(Rule), Prog(Prog), Goal(Goal))

class Pretty a where
  pretty :: a -> String


instance Pretty Term where
    pretty (Var (VarName s)) = s 
    pretty (Comb s []) =  s
    pretty (Comb s (t:ts)) = s ++ "(" ++ pretty t ++ foldr (\a p -> ", " ++ pretty a ++ p ) "" ts  ++")"

instance Pretty Rule where
    pretty (Rule term []) = pretty term 
    pretty (Rule term (t:ts)) = pretty term ++ " :- " ++ pretty t ++ foldr (\a p -> p ++ ", " ++ pretty a) "" ts

instance Pretty Prog where
    pretty (Prog []) = ""
    pretty (Prog [r]) = pretty r ++ "."
    pretty (Prog (r:rs)) = pretty r ++ ".\n" ++ pretty (Prog rs)

instance Pretty Goal where
    pretty (Goal []) = "?- ."
    pretty (Goal (t:ts)) = "?- " ++ pretty t ++ foldr (\a p -> p ++ ", " ++ pretty a) "" ts ++ "."