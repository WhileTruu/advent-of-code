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

    shortestPath = findShortestPathLen input
    sps = shortestPath |> Num.toStr

    Stdout.line
        """

        shortest path len: \(sps)

        """

Pos : { x : Nat, y : Nat }

findShortestPathLen : Str -> Nat
findShortestPathLen = \input ->
    graph = parseGrid input

    start = findStart graph
    end = findEnd graph

    findShortestPathDistance graph { start: end, end: "a" }

findShortestPathDistance : List (List Str), { start : Pos, end : Str } -> Nat
findShortestPathDistance = \graph, { start, end } ->
    findShortestPathDistanceHelp graph { end } [T start 0] (Set.single start)

findShortestPathDistanceHelp : List (List Str), { end : Str }, List [T Pos Nat], Set Pos -> Nat
findShortestPathDistanceHelp = \graph, { end }, queue, explored ->
    when queue is
        [] -> 420
        [T first distance, ..] ->
            rest = List.dropFirst queue

            elevationStr = getFromGraph graph first |> Result.withDefault ""
            elevation = elevationFromStr elevationStr |> Result.withDefault 100

            if elevationStr == end then
                distance
            else
                unexplored : Set Pos
                unexplored =
                    [{ x: -1, y: 0 }, { x: 1, y: 0 }, { x: 0, y: -1 }, { x: 0, y: 1 }]
                    |> List.keepOks \pos ->
                        xLimit = List.first graph |> Result.map List.len |> Result.withDefault 0
                        yLimit = List.len graph

                        x = (Num.toI64 first.x + pos.x) |> Num.toNatChecked |> Result.withDefault 0
                        y = (Num.toI64 first.y + pos.y) |> Num.toNatChecked |> Result.withDefault 0

                        posElevation =
                            getFromGraph graph { x, y }
                            |> Result.try elevationFromStr
                            |> Result.withDefault 200

                        if x < xLimit && y < yLimit && posElevation + 1 >= elevation then
                            Ok { x, y }
                        else
                            Err UnreachablePos
                    |> Set.fromList
                    |> \a -> Set.difference a explored

                newQueue =
                    unexplored
                    |> Set.toList
                    |> List.map \a -> T a (distance + 1)
                    |> \a -> List.concat rest a

                newExplored : Set Pos
                newExplored =
                    Set.union explored unexplored

                findShortestPathDistanceHelp graph { end } newQueue newExplored

getFromGraph : List (List Str), Pos -> Result Str [OutOfBounds]
getFromGraph = \graph, pos ->
    List.get graph pos.y
    |> Result.try (\a -> List.get a pos.x)

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

elevationFromStr : Str -> Result Nat [ListWasEmpty, OutOfBounds]
elevationFromStr = \value ->
    if value == "S" then
        Ok 0
    else if value == "E" then
        Ok 25
    else
        Str.toUtf8 value
        |> List.first
        |> Result.try \a -> Num.toNatChecked (a - 97)

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
