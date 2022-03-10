module Task2 where 

import Type
    ( Goal(..), Prog(..), Rule(..), Term(..), VarName(VarName) )

-- class to use pretty
class Pretty a where
  pretty :: a -> String

-- instance to retrieve a pretty term
instance Pretty Term where
    pretty (Var (VarName s)) = s 
    pretty (Comb s []) =  s
    pretty (Comb s (t:ts)) = if isPrologList (Comb s (t:ts))
                            then "[" ++ makeList (t:ts) ++ "]"
                            else s ++ "(" ++ pretty t ++ foldr (\a p -> ", " ++ pretty a ++ p ) "" ts  ++")"
     where 
         -- checks whether there is a nicer way to print the list or not
         isPrologList (Comb "." list) = length list == 2
         isPrologList _ = False
         
        --constructs the pretty list in prolog style
         makeList [t1 , (Comb "[]" [])]  = pretty t1
         makeList [t1 , (Comb s2 l)] = if isPrologList (Comb s2 l)
            then pretty t1 ++ ", "  ++  makeList l
            else pretty t1 ++ "|" ++ pretty (Comb s2 l)
         makeList [t1 , t2] = pretty t1 ++ "|" ++ pretty t2
         makeList _ = ""


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
