interface Utils.List
    exposes [bubbleSort]
    imports []

bubbleSort : List a, (a, a -> [GT, LT, EQ]) -> List a | a has Bool.Eq
bubbleSort = \list, order ->
    pass1 = bubbleSortPass list order []
    pass2 = bubbleSortPass pass1 order []

    if pass1 == pass2 then
        pass1
    else
        bubbleSort pass2 order

bubbleSortPass : List a, (a, a -> [GT, LT, EQ]), List a -> List a
bubbleSortPass = \list, order, passed ->
    when list is
        [] -> passed
        [first] -> List.append passed first
        [first, second, ..] ->
            others = list |> List.drop 2

            when order first second is
                GT -> bubbleSortPass (List.prepend others first) order (List.append passed second)
                _ -> bubbleSortPass (List.prepend others second) order (List.append passed first)
