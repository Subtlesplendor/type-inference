module [
    w,
]

import Exp exposing [Exp]
import State exposing [State]
import Type exposing [Type]
import Subst exposing [Subst]
import Scheme
import TypeEnv exposing [TypeEnv]

w : TypeEnv, Exp, State -> Result (Subst, Type, State) _
w = \gamma, e, s ->
    when e is
        EConst ->
            Ok (Subst.empty {}, TConst, s)

        EVar x ->
            scheme = try TypeEnv.get gamma x
            (newtype, newState) = Scheme.instantiate scheme s
            Ok (Subst.empty {}, newtype, newState)

        EAbs x e1 ->
            (newtype, newstate) = Type.fresh s
            newEnv = gamma |> TypeEnv.add x (Scheme.fromType newtype)
            (sub1, t1, state1) = try w newEnv e1 newstate
            fvar = newtype |> Subst.apply sub1
            Ok (sub1, TFun fvar t1, state1)

        EApp e1 e2 ->
            (sub1, t1, state1) = try w gamma e1 s
            newEnv = gamma |> TypeEnv.applySubst sub1
            (sub2, t2, state2) = try w newEnv e2 state1
            (beta, state3) = Type.fresh state2
            sub3 = try Subst.mgu (t1 |> Subst.apply sub2) (TFun t2 beta)
            newSub =
                sub1
                |> Subst.compose sub2
                |> Subst.compose sub3
            Ok (newSub, beta |> Subst.apply sub3, state3)

        ELet x e1 e2 ->
            (sub1, t1, state1) = try w gamma e1 s
            newEnv = gamma |> TypeEnv.applySubst sub1
            scheme = t1 |> TypeEnv.generalize newEnv
            (sub2, t2, state2) = try w (newEnv |> TypeEnv.add x scheme) e2 state1

            Ok (sub1 |> Subst.compose sub2, t2, state2)

expect
    #    init = State.initial
    gamma = TypeEnv.empty {}
    expr = ELet "id" (EAbs "x" (EVar "x")) (EApp (EApp (EVar "id") (EVar "id")) (EApp (EVar "id") (EVar "id")))

    res = w gamma expr (State.initial)
    when res is
        Ok (_, type, _) ->
            type == (TFun (TVar "a6") (TVar "a6"))

        Err _ -> Bool.false
