app "day-14-part-2"
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

    rawPaths = parseRawPaths exampleInput
    rawBounds = calcRawBounds rawPaths
    rawSandPosition = { x: 500, y: 0 }

    paths = translatePathsToZeroBasedGrid rawPaths
    sandPosition = {
        x: rawSandPosition.x - rawBounds.minX,
        y: rawSandPosition.y - rawBounds.minY,
    }
    { grid, count } =
        createGrid paths
        |> addSand sandPosition
        |> dropSandUntilAbyss sandPosition 0

    drawnGrid = drawGrid grid
    countStr = Num.toStr count

    Stdout.line
        """
        \(drawnGrid)
        \(countStr)
        """

dropSandUntilAbyss : List (List Str), { x : Nat, y : Nat }, Nat -> { count : Nat, grid : List (List Str) }
dropSandUntilAbyss = \grid, sand, count ->
    stuff = dropSand grid sand

    if grid == stuff.grid || count == 50 then
        { count, grid }
    else
        dropSandUntilAbyss stuff.grid { sand & x: stuff.x } (count + 1)

dropSand : List (List Str), { x : Nat, y : Nat } -> { grid: List (List Str), x: Nat }
dropSand = \grid, sand ->
    grid
    |> List.walkUntil (T [] { i: 0, x: sand.x, translatedX: sand.x }) \T rows { i, x, translatedX }, row ->
        if i == sand.y then
            Continue (T (List.append rows row) { i: i + 1, x, translatedX })
        else
            when List.get grid (i + 1) is
                Ok nextRow ->
                    when getNextSandX nextRow x is
                        Ok newX ->
                            Continue (T (List.append rows row) { i: i + 1, x: newX, translatedX })

                        Err AbyssLeft ->
                            newGrid =
                                rows
                                |> List.append row
                                |> List.concat (List.split grid (i + 1) |> .others)
                                |> List.walk (T2 [] 0) \T2 state index, r ->
                                    if index == List.len grid - 1 then
                                        T2 (List.append state (List.prepend r "#")) (index + 1)
                                    else
                                        T2 (List.append state (List.prepend r ".")) (index + 1)
                                |> \T2 a _ -> a

                            Break (T newGrid { i: i + 1, x, translatedX: sand.x + 1 })

                        Err AbyssRight ->
                            newGrid =
                                rows
                                |> List.append row
                                |> List.concat (List.split grid (i + 1) |> .others)
                                |> List.mapWithIndex \r, ri ->
                                    if ri == List.len grid - 1 then
                                        List.append r "#"
                                    else
                                        List.append r "."

                            Break (T newGrid { i: i + 1, x, translatedX })

                        Err Bottom ->
                            sandyRow : List Str
                            sandyRow = List.set row x "o"

                            newGrid =
                                rows
                                |> List.append sandyRow
                                |> List.concat (List.split grid (i + 1) |> .others)

                            Break (T newGrid { i: i + 1, x, translatedX })

                _ ->
                    newGrid =
                        rows
                        |> List.append row
                        |> List.concat (List.split grid (i + 1) |> .others)

                    Break (T newGrid { i: i + 1, x, translatedX })
    |> \T a { translatedX } -> { grid: a, x: translatedX }

getNextSandX : List Str, Nat -> Result Nat [Bottom, AbyssLeft, AbyssRight]
getNextSandX = \next, x ->
    orList
        (getValidSandPos next x)
        (
            if x == 0 then
                [Err AbyssLeft]
            else
                [
                    getValidSandPos next (x - 1),
                    if x >= List.len next then
                        Err AbyssRight
                    else
                        getValidSandPos next (x + 1),
                ]
        )

getValidSandPos : List Str, Nat -> Result Nat [Bottom]
getValidSandPos = \row, x ->
    when List.get row x is
        Ok "." ->
            Ok x

        _ ->
            Err Bottom


orList : Result a e, List (Result a e) -> Result a e
orList = \result, results ->
    when result is
        Ok _ -> result
        Err _ ->
            when results is
                [] -> result
                [first, ..] -> orList first (List.dropFirst results)

parseRawPaths : Str -> List (List { x : Nat, y : Nat })
parseRawPaths = \input -> input |> Str.split "\n" |> List.map parsePath

translatePathsToZeroBasedGrid : List (List { x : Nat, y : Nat }) -> List (List { x : Nat, y : Nat })
translatePathsToZeroBasedGrid = \paths ->
    bounds = calcRawBounds paths

    paths
    |> List.map \path ->
        path
        |> List.map \point -> {
            x: point.x - bounds.minX,
            y: point.y - bounds.minY,
        }

parsePath : Str -> List { x : Nat, y : Nat }
parsePath = \input ->
    input
    |> Str.split " -> "
    |> List.map \pointStr ->
        when Str.split pointStr "," |> List.map Str.toNat is
            [Ok x, Ok y] -> { x, y }
            _ -> crash "Could not parse point."

createGrid : List (List { x : Nat, y : Nat }) -> List (List Str)
createGrid = \paths ->
    bounds = calcBounds paths

    row : List Str
    row = List.repeat "." (bounds.maxX - bounds.minX + 1)

    grid : List (List Str)
    grid =
        List.repeat row (bounds.maxY - bounds.minY + 1)
        |> List.append row
        |> List.append (List.repeat "#" (bounds.maxX - bounds.minX + 1))

    paths
    |> List.walk grid addPathToGrid

drawGrid : List (List Str) -> Str
drawGrid = \grid -> grid
    |> List.map (\a -> Str.joinWith a "")
    |> Str.joinWith "\n"

addSand : List (List Str), { x : Nat, y : Nat } -> List (List Str)
addSand = \grid, sandPosition ->
    List.mapWithIndex grid \row, i ->
        if i == sandPosition.y then
            List.set row sandPosition.x "+"
        else
            row

addPathToGrid : List (List Str), List { x : Nat, y : Nat } -> List (List Str)
addPathToGrid = \grid, path ->
    others = List.drop path 2

    when path is
        [first, second, ..] ->
            if first.x == second.x then
                newGrid =
                    List.mapWithIndex grid \row, i ->
                        # FIXME the order of second and first, could it be reversed too?
                        if i >= first.y && i <= second.y then
                            List.set row first.x "#"
                        else if i >= second.y && i <= first.y then
                            List.set row first.x "#"
                        else
                            row

                addPathToGrid newGrid (List.prepend others second)
            else if first.y == second.y then
                newGrid =
                    List.mapWithIndex grid \row, i ->
                        if i == first.y then
                            List.mapWithIndex row \cell, j ->
                                # FIXME the order of second and first, could it be reversed too?
                                if j >= first.x && j <= second.x then
                                    "#"
                                else if j >= second.x && j <= first.x then
                                    "#"
                                else
                                    cell
                        else
                            row

                addPathToGrid newGrid (List.prepend others second)
            else
                addPathToGrid grid (List.prepend others second)

        _ -> grid

calcBounds : List (List { x : Nat, y : Nat }) -> { minX : Nat, maxX : Nat, minY : Nat, maxY : Nat }
calcBounds = \paths ->
    minY = 0
    minX = 0

    boundsResult =
        maxX <- paths |> List.map (\a -> List.map a .x) |> List.join |> List.max |> Result.try
        maxY <- paths |> List.map (\a -> List.map a .y) |> List.join |> List.max |> Result.map

        { minX, maxX, minY, maxY }

    when boundsResult is
        Ok a -> a
        Err _ -> crash "Could not get min and max."

calcRawBounds : List (List { x : Nat, y : Nat }) -> { minX : Nat, maxX : Nat, minY : Nat, maxY : Nat }
calcRawBounds = \paths ->
    minY = 0

    boundsResult =
        minX <- paths |> List.map (\a -> List.map a .x) |> List.join |> List.min |> Result.try
        maxX <- paths |> List.map (\a -> List.map a .x) |> List.join |> List.max |> Result.try
        maxY <- paths |> List.map (\a -> List.map a .y) |> List.join |> List.max |> Result.map

        { minX, maxX, minY, maxY }

    when boundsResult is
        Ok a -> a
        Err _ -> crash "Could not get min and max."

exampleInput =
    """
    498,4 -> 498,6 -> 496,6
    503,4 -> 502,4 -> 502,9 -> 494,9
    """

expect
    rawPaths = parseRawPaths exampleInput
    rawBounds = calcRawBounds rawPaths
    rawSandPosition = { x: 500, y: 0 }

    paths = translatePathsToZeroBasedGrid rawPaths
    sandPosition = {
        x: rawSandPosition.x - rawBounds.minX,
        y: rawSandPosition.y - rawBounds.minY,
    }
    { count } =
        createGrid paths
        |> addSand sandPosition
        |> dropSandUntilAbyss sandPosition 0

    count == 24
