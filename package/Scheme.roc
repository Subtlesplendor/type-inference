module [
    Scheme,
    fromType,
    freevars,
    applySubst,
    instantiate,
]

import Type exposing [Type]
import Subst exposing [Subst]
import State exposing [State]

# A scheme is a type together with a set of bound type variables (to the universal quantifier, i.e. forall)
Scheme : [Scheme (Set Str) Type]

fromType : Type -> Scheme
fromType = \t -> Scheme (Set.empty {}) t

# The free variables of a scheme consists of the free variables of the types, with the bound type variables removed
freevars : Scheme -> Set Str
freevars = \Scheme vars t ->
    (Type.freevars t) |> Set.difference vars

# To apply a substitution to a scheme means to remove all the bound variables from the substitution first.
applySubst : Subst, Scheme -> Scheme
applySubst = \subst, Scheme vars t ->
    newSubstition =
        vars
        |> Set.walk subst Subst.remove
    Scheme vars (Subst.apply newSubstition t)

# Instantiating a type scheme replaces all the bound variables with fresh names.
instantiate : Scheme, State -> (Type, State)
instantiate = \Scheme vars type, state ->
    (newSubst, newState) =
        vars
        |> Set.walk (Subst.empty {}, state) \(subst, s), var ->
            newType = s |> State.name |> TVar
            substp = subst |> Subst.insert var newType
            (substp, State.update s)

    (Subst.apply newSubst type, newState)
