module [
    TypeEnv,
    empty,
    freevars,
    applySubst,
    remove,
    generalize,
    add,
    get,
]

import Subst exposing [Subst]
import Type exposing [Type]
import Scheme exposing [Scheme]

# A Type environmnet is a map from type variables to type schemes
TypeEnv := Dict Str Scheme

empty : {} -> TypeEnv
empty = \{} -> Dict.empty {} |> @TypeEnv

remove : TypeEnv, Str -> TypeEnv
remove = \@TypeEnv env, var ->
    @TypeEnv (env |> Dict.remove var)

# The free variables of a type environment is the union of the free variables in the type schemes
freevars : TypeEnv -> Set Str
freevars = \@TypeEnv env ->
    env
    |> Dict.values
    |> List.map Scheme.freevars
    |> List.walk (Set.empty {}) Set.union

# Applying a substition to a type environment means applying the substitution to the type schemes
applySubst : TypeEnv, Subst -> TypeEnv
applySubst = \@TypeEnv env, s ->
    env
    |> Dict.map (\_, scheme -> Scheme.applySubst s scheme)
    |> @TypeEnv

# generalizing a type in a type environment means quantification of type variables free in the type but not in the env
generalize : Type, TypeEnv -> Scheme
generalize = \type, typeEnv ->
    vars =
        Type.freevars type
        |> Set.difference (freevars typeEnv)
    Scheme vars type

add : TypeEnv, Str, Scheme -> TypeEnv
add = \@TypeEnv env, var, scheme ->
    env
    |> Dict.insert var scheme
    |> @TypeEnv

get : TypeEnv, Str -> Result Scheme [VarNotInEnv]
get = \@TypeEnv env, var -> env |> Dict.get var |> Result.mapErr \_ -> VarNotInEnv
