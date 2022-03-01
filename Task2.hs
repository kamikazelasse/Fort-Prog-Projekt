module Task2 where

import Type
    ( Goal(..), Prog(..), Rule(..), Term(..), VarName(VarName) ) 
class Pretty a where
  pretty :: a -> String


instance Pretty Term where
    pretty (Var (VarName s)) = s 
    pretty (Comb s []) =  s
    pretty (Comb s (t:ts)) = if isPrologList (Comb s (t:ts))
                            then "[" ++ makeList (t:ts) "" ++ "]"
                            else s ++ "(" ++ pretty t ++ foldr (\a p -> ", " ++ pretty a ++ p ) "" ts  ++")"
     where 
         isPrologList (Comb "." list) = length list == 2
         isPrologList (Comb _ list) = False

         makeList [(Var s), term2] done = pretty (Var s) ++ "|" ++ pretty term2 ++ done
         makeList [ term , (Comb "[]" [])] done = pretty term ++ done
         makeList [ term , (Comb "." list) ] done = 
             if isPrologList (Comb "." list) 
                 then pretty term ++ ", " ++ makeList list done 
                 else pretty term ++ ", " ++ pretty (Comb "." list)    
         makeList [term , term2] done = pretty term ++ "|" ++ pretty term2 ++ done

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
