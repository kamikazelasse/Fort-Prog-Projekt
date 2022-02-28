import Type (VarName(VarName), CombName, Term(Var, Comb), Rule(Rule), Prog(Prog), Goal(Goal))  

class Pretty a where
  pretty :: a -> String


instance Pretty Type.Term where
    pretty (Type.Var (Type.VarName s)) = s 
    pretty (Type.Comb s []) =  s
    pretty (Type.Comb s (t:ts)) = s Prelude.++ "(" Prelude.++ pretty t Prelude.++ Prelude.foldr (\a p -> ", " Prelude.++ pretty a Prelude.++ p ) "" ts  Prelude.++")"

instance Pretty Type.Rule where
    pretty (Type.Rule term []) = pretty term 
    pretty (Type.Rule term (t:ts)) = pretty term Prelude.++ " :- " Prelude.++ pretty t Prelude.++ Prelude.foldr (\a p -> p Prelude.++ ", " Prelude.++ pretty a) "" ts

instance Pretty Type.Prog where
    pretty (Type.Prog []) = ""
    pretty (Type.Prog [r]) = pretty r Prelude.++ "."
    pretty (Type.Prog (r:rs)) = pretty r Prelude.++ ".\n" Prelude.++ pretty (Type.Prog rs)

instance Pretty Type.Goal where
    pretty (Type.Goal []) = "?- ."
    pretty (Type.Goal (t:ts)) = "?- " Prelude.++ pretty t Prelude.++ Prelude.foldr (\a p -> p Prelude.++ ", " Prelude.++ pretty a) "" ts Prelude.++ "."