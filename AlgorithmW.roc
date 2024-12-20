module [
    Lit,
    Exp,
    w,
]

import State exposing [State]
import Type exposing [Type]
import Subst exposing [Subst]
import Scheme
import TypeEnv exposing [TypeEnv]

Lit : [LInt I32]

Exp : [
    ELit Lit,
    EVar Str,
    EAbs Str Exp,
    EApp Exp Exp,
    ELet Str Exp Exp,
]

w : TypeEnv, Exp, State -> Result (Subst, Type, State) _
w = \gamma, e, s ->
    when e is
        ELit _ ->
            Ok (Subst.empty {}, TInt, s)

        EVar x ->
            scheme = TypeEnv.get? gamma x
            (newtype, newState) = Scheme.instantiate scheme s
            Ok (Subst.empty {}, newtype, newState)

        EAbs x e1 ->
            (newtype, newstate) = Type.fresh s
            newEnv = gamma |> TypeEnv.add x (Scheme.fromType newtype)
            (sub1, t1, state1) = w? newEnv e1 newstate
            fvar = sub1 |> Subst.apply newtype
            Ok (sub1, TFun fvar t1, state1)

        EApp e1 e2 ->
            (sub1, t1, state1) = w? gamma e1 s
            newEnv = TypeEnv.applySubst sub1 gamma
            (sub2, t2, state2) = w? newEnv e2 state1
            (beta, state3) = Type.fresh state2
            sub3 = Subst.mgu? (sub2 |> Subst.apply t1) (TFun t2 beta)
            newSub =
                sub1
                |> Subst.compose sub2
                |> Subst.compose sub3
            Ok (newSub, sub3 |> Subst.apply beta, state3)

        ELet x e1 e2 ->
            (sub1, t1, state1) = w? gamma e1 s
            newEnv = sub1 |> TypeEnv.applySubst gamma
            scheme = newEnv |> TypeEnv.generalize t1
            (sub2, t2, state2) = w? (newEnv |> TypeEnv.add x scheme) e2 state1

            Ok (sub1 |> Subst.compose sub2, t2, state2)
