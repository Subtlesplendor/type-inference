module [
    m,
]

import Exp exposing [Exp]
import State exposing [State]
import Type exposing [Type]
import Subst exposing [Subst]
import Scheme
import TypeEnv exposing [TypeEnv]

m : TypeEnv, Exp, Type, State -> Result (Subst, State) _
m = \gamma, e, type, s ->
    when e is
        EConst ->
            subst = try Subst.mgu type TConst
            Ok (subst, s)

        EVar x ->
            scheme = try TypeEnv.get gamma x
            (newtype, newState) = Scheme.instantiate scheme s
            newSubst = try Subst.mgu type newtype
            Ok (newSubst, newState)

        EAbs x e1 ->
            (beta1, state1) = Type.fresh s
            (beta2, state2) = Type.fresh state1
            subst1 = try Subst.mgu type (TFun beta1 beta2)
            newEnv =
                gamma
                |> TypeEnv.add x (Scheme.fromType beta1)
                |> TypeEnv.applySubst subst1
            newBeta2 = beta2 |> Subst.apply subst1
            (subst2, state3) = try m newEnv e1 newBeta2 state2
            Ok (subst1 |> Subst.compose subst2, state3)

        EApp e1 e2 ->
            (beta, state1) = Type.fresh s
            (sub1, state2) = try m gamma e1 (TFun beta type) state1
            newEnv = gamma |> TypeEnv.applySubst sub1
            newBeta = beta |> Subst.apply sub1
            (sub2, state3) = try m newEnv e2 newBeta state2
            Ok (sub1 |> Subst.compose sub2, state3)

        ELet x e1 e2 ->
            (beta, state1) = Type.fresh s
            (sub1, state2) = try m gamma e1 beta state1
            env1 = gamma |> TypeEnv.applySubst sub1
            scheme =
                beta
                |> Subst.apply sub1
                |> TypeEnv.generalize env1
            newEnv = env1 |> TypeEnv.add x scheme
            newType = type |> Subst.apply sub1
            (sub2, state3) = try m newEnv e2 newType state2
            Ok (sub1 |> Subst.compose sub2, state3)
