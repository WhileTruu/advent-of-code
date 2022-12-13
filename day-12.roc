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
    (T shortestPath graphStr) = findShortestPathLen exampleInput
    sps = shortestPath |> Num.toStr

    Stdout.line
        """

        shortest path len: \(sps)

        """

Pos : { x : Nat, y : Nat }

findShortestPathLen : Str -> [T Nat Str]
findShortestPathLen = \input ->
    grid = parseGrid input

    end = findEnd grid
    start = findStart grid

    graph : Dict Pos (Set Pos)
    graph = createGraph grid

    v = graphToStr graph
    x = findPaths graph { start, end }

    z = List.map x List.len |> List.sortAsc |> List.first |> Result.withDefault 420
    dbg (x |> List.map (\a -> Str.joinWith (List.map a posToStr) ", ") |> Str.joinWith "\n")
    T z v

findPaths : Dict Pos (Set Pos), { start : Pos, end : Pos } -> List (List Pos)
findPaths = \graph, { start, end } ->
    findPathsHelp graph { end } [T start []] [start] []

findPathsHelp : Dict Pos (Set Pos), { end : Pos }, List [T Pos (List Pos)], List Pos, List (List Pos) -> List (List Pos)
findPathsHelp = \graph, { end }, queue, explored, paths ->
    #dbg queue
    when queue is
        [] -> paths
        [T fst path, ..] ->
            rest = List.dropFirst queue
            newPath = (List.append path fst)

            if fst == end then
                [newPath]

            # else if List.any paths (\a -> List.startsWith a path) then
            #     findPathsHelp graph { end } rest paths

            else
                unexplored =
                    Dict.get graph fst
                    |> Result.map Set.toList
                    |> Result.withDefault []
                    |> List.dropIf (\a -> List.contains explored a)

                findPathsHelp graph { end }
                    (List.concat rest (List.map unexplored \a -> T a newPath))
                    (List.concat explored unexplored)
                    paths

createGraph : List (List Str) -> Dict Pos (Set Pos)
createGraph = \grid ->
    grid
    |> List.walk (T1 Dict.empty 0) \T1 state1 y, line ->
        line
        |> List.walk (T2 Dict.empty 0) \T2 state2 x, value ->
            getAdjacentCells { x: x, y: y } value grid
            |> Set.fromList
            |> \a -> Dict.insert state2 { x, y } a
            |> \a -> T2 a (x + 1)
        |> \T2 a _ -> T1 (Dict.insertAll state1 a) (y + 1)
    |> \T1 a _ -> a

graphToStr : Dict Pos (Set Pos) -> Str
graphToStr = \graph ->
    Dict.toList graph
    |> List.map \T pos set ->
        posStr = posToStr pos
        posListStr = List.map (Set.toList set) posToStr |> Str.joinWith ", "

        "\(posStr): [\(posListStr)]"
    |> Str.joinWith "\n"

posToStr : Pos -> Str
posToStr = \{ x, y } ->
    xStr = Num.toStr x
    yStr = Num.toStr y

    "(\(xStr), \(yStr))"

getAdjacentCells : Pos, Str, List (List Str) -> List Pos
getAdjacentCells = \pos, value, grid ->
    xMax =
        List.first grid
        |> Result.map \a -> List.len a - 1
        |> Result.withDefault 0

    yMax = List.len grid - 1

    [
        if pos.x == 0 then Err NoLeft else Ok { pos & x: pos.x - 1 },
        if pos.x == xMax then Err NoRight else Ok { pos & x: pos.x + 1 },
        if pos.y == 0 then Err NoUp else Ok { pos & y: pos.y - 1 },
        if pos.y == yMax then Err NoUp else Ok { pos & y: pos.y + 1 },
    ]
    |> List.keepOks \a -> a
    |> List.keepIf \a ->
        a1 = Num.toI64 (elevationFromStr value)
        a2 =
            List.get grid a.y
            |> Result.try \line -> List.get line a.x
            |> Result.map elevationFromStr
            |> Result.withDefault 100
            |> Num.toI64

        a2 - a1 <= 1

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

elevationFromStr : Str -> Nat
elevationFromStr = \value ->
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
