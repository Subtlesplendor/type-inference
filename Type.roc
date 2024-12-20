module [
    Type,
    freevars,
    fresh,
]

import State exposing [State]

# A Type is either a type variable, a concrete type, or a function type.
Type : [TVar Str, TInt, TFun Type Type]

# freevars is the set of free type variables
freevars : Type -> Set Str
freevars = \type ->
    when type is
        TVar a -> Set.single a
        TInt -> Set.empty {}
        TFun t1 t2 -> freevars t1 |> Set.union (freevars t2)

fresh : State -> (Type, State)
fresh = \s -> (State.name s |> TVar, State.update s)
