app "day-14"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [
        pf.Stdout,
        pf.File,
        pf.Task.{ await, Task },
        pf.Path.{ Path },
    ]
    provides [main] to pf

main : Task {} *
main =
    input <- File.readUtf8 (Path.fromStr "day-14-input.txt")
        |> Task.onFail \_ -> crash "Could not read file."
        |> Task.await

    paths = parsePaths exampleInput
    dbg paths |> List.map (\a -> List.map a .y) |> List.join |> List.min

    drawnPaths = drawPaths paths

    Stdout.line
        """
        \(drawnPaths)
        """

parsePaths : Str -> List (List { x: Nat, y: Nat })
parsePaths = \input -> input |> Str.split "\n" |> List.map parsePath

parsePath : Str -> List { x: Nat, y: Nat }
parsePath = \input ->
    input
    |> Str.split " -> "
    |> List.map \pointStr ->
        when Str.split pointStr "," |> List.map Str.toNat is
            [Ok x, Ok y] -> { x, y }
            _ -> crash "Could not parse point."

exampleInput =
    """
    498,4 -> 498,6 -> 496,6
    503,4 -> 502,4 -> 502,9 -> 494,9
    """

drawPaths : List (List { x: Nat, y: Nat }) -> Str
drawPaths = \paths ->
    bounds = calcBounds paths

    row : List Str
    row = List.repeat "." (bounds.maxX - bounds.minX + 1)

    List.repeat row (bounds.maxY - bounds.minY + 1)
    |> List.map (\a -> Str.joinWith a "")
    |> Str.joinWith "\n"

calcBounds : List (List { x: Nat, y: Nat }) -> { minX: Nat, maxX: Nat, minY: Nat, maxY: Nat }
calcBounds = \paths ->
    minY = 0

    boundsResult =
        minX <- paths |> List.map (\a -> List.map a .x) |> List.join |> List.min |> Result.try
        maxX <- paths |> List.map (\a -> List.map a .x) |> List.join |> List.max |> Result.try
        maxY <- paths |> List.map (\a -> List.map a .y) |> List.join |> List.max |> Result.map

        { minX, maxX, minY, maxY }

    when boundsResult is
        Ok a -> a
        Err _ -> crash "Could not get min and max."