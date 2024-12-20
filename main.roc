app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout

import Type
import Subst
import Scheme
import TypeEnv

# import AlgorithmW

# s1 = AlgorithmW.ftv (TVar "a")

main =
    # newstr = if (Set.single "a" == s1) then "true" else "false"
    str =
        "Hi there, from inside a Roc app. 🎉\n"
    #    |> Str.concat newstr
    Stdout.line! str
