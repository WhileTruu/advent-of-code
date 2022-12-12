app "day-12"
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
    input <- File.readUtf8 (Path.fromStr "day-12-input.txt")
        |> Task.onFail \_ -> crash "Could not read file."
        |> Task.await

    # shortestExamplePathLenStr = findShortestPathLen exampleInput |> Num.toStr
    shortestPathLenStr = findShortestPathLen input |> Num.toStr

    v =
        input
        |> parseGrid
        |> gridToElevationGrid
        |> List.map \line ->
            List.map line Num.toStr |> Str.joinWith " "
        |> Str.joinWith "\n"

    Stdout.line
        """

        shortest path len: \(shortestPathLenStr)
        \(v)
        """

Pos : { x : Nat, y : Nat }

findShortestPathLen : Str -> Nat
findShortestPathLen = \input ->
    grid = parseGrid input
    elevationGrid = gridToElevationGrid grid

    end = findEnd grid
    start = findStart grid

    cellsWithCounts = calcSomething { start, grid: elevationGrid, before: [], current: T end 0, after: [] }

    shortestPath = getShortestPath { end, cellsWithCounts, grid: elevationGrid } [] start

    # dbg (List.findLast cellsWithCounts \T a _ -> a.x < 30)

    (List.len shortestPath) - 1

getShortestPath :{
        end : Pos,
        cellsWithCounts : List [T Pos Nat],
        grid : List (List Nat),
    },
    List Pos,
    Pos
    -> List Pos
getShortestPath = \{ end, cellsWithCounts, grid }, path, pos ->
    if pos == end then
        List.append path pos
    else
        xMax =
            List.first grid
            |> Result.map \a -> List.len a - 1
            |> Result.withDefault 0

        yMax = List.len grid - 1

        adjacentCells =
            getAdjacentCells pos { xMax, yMax }
            |> List.keepIf \a ->
                a1 = Num.toI64 (getElevation pos grid)
                a2 = Num.toI64 (getElevation a grid)

                Num.abs (a1 - a2) <= 1
            |> List.walk [] \state, adjacentPos ->
                List.findFirst cellsWithCounts \T a _ -> a == adjacentPos
                |> Result.map (\a -> List.append state a)
                |> Result.withDefault state
            |> List.sortWith \T _ a1, T _ a2 -> Num.compare a1 a2

        # dbg (getAdjacentCells pos { xMax, yMax })

        when adjacentCells is
            [] -> List.append path pos
            [T fst _, ..] ->
                getShortestPath
                    { end, cellsWithCounts, grid }
                    (List.append path pos)
                    fst

calcSomething :{
        start : Pos,
        grid : List (List Nat),

        # Main
        before : List [T Pos Nat],
        current : [T Pos Nat],
        after : List [T Pos Nat],
    }
    -> List [T Pos Nat]
calcSomething = \{ start, grid, before, current, after } ->
    (T pos counter) = current

    if pos == start then
        List.append before current
    else
        xMax =
            List.first grid
            |> Result.map \a -> List.len a - 1
            |> Result.withDefault 0

        yMax = List.len grid - 1

        posElevation = getElevation pos grid

        adjacentCells =
            getAdjacentCells pos { xMax, yMax }
            |> List.keepIf \a ->
                a1 = Num.toI64 posElevation
                a2 = Num.toI64 (getElevation a grid)

                Num.abs (a1 - a2) <= 1
            |> List.dropIf \a ->
                List.any before (\T mainA _ -> mainA == a) || List.any after (\T mainA _ -> mainA == a)
            |> List.map (\a -> T a (counter + 1))

        newBefore = List.append before (T pos counter)
        newAfter = List.concat after adjacentCells

        when newAfter is
            [] -> List.append before current
            [fst, ..] ->
                calcSomething {
                    start,
                    grid,
                    before: newBefore,
                    current: fst,
                    after: List.dropFirst newAfter,
                }

getElevation : Pos, List (List Nat) -> Nat
getElevation = \pos, grid ->
    List.get grid pos.y
    |> Result.try \a -> List.get a pos.x
    |> Result.withDefault 100

getAdjacentCells : Pos, { xMax : Nat, yMax : Nat } -> List Pos
getAdjacentCells = \pos, { xMax, yMax } ->
    [
        if pos.x == 0 then Err NoLeft else Ok { pos & x: pos.x - 1 },
        if pos.x == xMax then Err NoRight else Ok { pos & x: pos.x + 1 },
        if pos.y == 0 then Err NoUp else Ok { pos & y: pos.y - 1 },
        if pos.y == yMax then Err NoUp else Ok { pos & y: pos.y + 1 },
    ]
    |> List.keepOks \a -> a

parseGrid : Str -> List (List Str)
parseGrid = \input ->
    input
    |> Str.split "\n"
    |> List.map Str.graphemes

findEnd : List (List Str) -> Pos
findEnd = \grid ->
    List.walkUntil grid { x: 0, y: 0 } \state, line ->
        List.findFirstIndex line (\a -> a == "E")
        |> Result.map (\a -> Break { state & x: a })
        |> Result.withDefault (Continue { state & y: state.y + 1 })

findStart : List (List Str) -> Pos
findStart = \grid ->
    List.walkUntil grid { x: 0, y: 0 } \state, line ->
        List.findFirstIndex line (\a -> a == "S")
        |> Result.map (\a -> Break { state & x: a })
        |> Result.withDefault (Continue { state & y: state.y + 1 })

gridToElevationGrid : List (List Str) -> List (List Nat)
gridToElevationGrid = \grid ->
    List.map grid \line ->
        List.map line \value ->
            if value == "S" then
                0
            else if value == "E" then
                25
            else
                Str.toUtf8 value
                |> List.first
                |> Result.map \a -> Num.toNat (a - 97)
                |> Result.withDefault 100

exampleInput : Str
exampleInput =
    """
    Sabqponm
    abcryxxl
    accszExk
    acctuvwj
    abdefghi
    """

expect findShortestPathLen exampleInput == 31
