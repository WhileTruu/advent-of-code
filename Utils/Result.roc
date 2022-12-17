interface Utils.Result
    exposes [map2, withDefaultLazy]
    imports []

map2 : Result a err, Result b err, (a, b -> c) -> Result c err
map2 = \r1, r2, f ->
    when r1 is
        Ok a1 ->
            when r2 is
                Ok a2 ->
                    Ok (f a1 a2)

                Err e2 ->
                    Err e2

        Err e1 ->
            Err e1

withDefaultLazy : Result a e, ({} -> a) -> a
withDefaultLazy = \r, default ->
    when r is
        Err _ -> default {}
        Ok a -> a
