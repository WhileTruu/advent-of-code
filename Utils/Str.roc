interface Utils.Str
    exposes [padLeft]
    imports []

padLeft : Str, Nat, Str -> Str
padLeft = \str, amount, padStr ->
    padding = List.repeat padStr amount |> Str.joinWith ""

    "\(padding)\(str)"
