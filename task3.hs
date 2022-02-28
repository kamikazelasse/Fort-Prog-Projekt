import Type (VarName(VarName), CombName, Term(Var, Comb), Rule(Rule), Prog(Prog), Goal(Goal))

class Vars a where
    allVars :: a -> [VarName]

instance Vars Term where
    allVars (Var name) 