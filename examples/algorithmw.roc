app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    algorithmW: "../package/main.roc",
}

import pf.Stdout

# import Type
# import Subst
# import Scheme
import TypeEnv

import State
import W exposing [w]

# init = State.initial
# gamma = TypeEnv.empty {}
# s1 = w gamma (EVar "x") init

init = State.initial
gamma = TypeEnv.empty {}
expr = ELet "id" (EAbs "x" (EVar "x")) (EApp (EApp (EVar "id") (EVar "id")) (EApp (EVar "id") (EVar "id")))

main =
    result =
        (_, type, _) = try w gamma expr init
        Ok type

    when result is
        Ok s -> Stdout.line! (Inspect.toStr s)
        _ -> Stdout.line! "failed"
