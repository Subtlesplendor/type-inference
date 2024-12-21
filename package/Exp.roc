module [
    Exp,
    Lit,
]

Lit : [LInt I32]

Exp : [
    ELit Lit,
    EVar Str,
    EAbs Str Exp,
    EApp Exp Exp,
    ELet Str Exp Exp,
]
