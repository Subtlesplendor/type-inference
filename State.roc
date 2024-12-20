module [
    State,
    initial,
    update,
    name,
]

# State
State := {
    id : U32,
}

update : State -> State
update = \@State { id } -> @State { id: id + 1 }

initial : State
initial = @State { id: 1 }

name : State -> Str
name = \@State { id } -> "a" |> Str.concat (Inspect.toStr id)
