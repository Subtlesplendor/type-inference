module [
    Subst,
    empty,
    single,
    get,
    insert,
    remove,
    apply,
    compose,
    mgu,
]

import Type exposing [Type]

# A substitution maps type-variable names to types
Subst := Dict Str Type

empty : {} -> Subst
empty = \{} -> Dict.empty {} |> @Subst

single : Str, Type -> Subst
single = \u, t -> Dict.single u t |> @Subst

get : Subst, Str -> Result Type [NoSubstForVar]
get = \@Subst dict, x ->
    dict |> Dict.get x |> Result.onErr \_ -> Err NoSubstForVar

insert : Subst, Str, Type -> Subst
insert = \@Subst dict, x, t ->
    dict |> Dict.insert x t |> @Subst

remove : Subst, Str -> Subst
remove = \@Subst dict, var ->
    dict |> Dict.remove var |> @Subst

# apply applies substitution to a type
apply : Type, Subst -> Type
apply = \type, subst ->
    when type is
        TVar n -> subst |> get n |> Result.withDefault (TVar n)
        TConst -> TConst
        TFun t1 t2 -> TFun (t1 |> apply subst) (t2 |> apply subst)

compose : Subst, Subst -> Subst
compose = \subst2, @Subst sdir1 ->
    (@Subst sdir2) = subst2
    sdir1
    |> Dict.map \_, type -> type |> apply subst2
    |> Dict.insertAll sdir2
    |> @Subst

mgu : Type, Type -> Result Subst _
mgu = \t1, t2 ->
    when (t1, t2) is
        (TFun l1 r1, TFun l2 r2) ->
            sub1 = try mgu l1 l2
            sub2 = try mgu (r1 |> apply sub1) (r2 |> apply sub1)
            Ok (Subst.compose sub1 sub2)

        (TVar u, t) -> varBind u t
        (t, TVar u) -> varBind u t
        (TConst, TConst) -> Ok (Subst.empty {})
        _ -> Err TypesDoNotUnify

varBind : Str, Type -> Result Subst _
varBind = \u, type ->
    when type is
        TVar y if y == u -> Ok (Subst.empty {})
        t if Set.contains (Type.freevars t) u -> Err FailsOccursCheck
        _ -> Ok (single u type)
