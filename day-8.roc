app "day-8"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [
        pf.Stdout,
        pf.File,
        pf.Task.{ await, Task },
        pf.Path.{ Path },
    ]
    provides [main] to pf

main : Task {} *
main =
    input <- File.readUtf8 (Path.fromStr "day-8-input.txt")
        |> Task.onFail \_ -> crash "Could not read file."
        |> Task.await

    exampleNumTreesVisibleOutsideGridStr : Str
    exampleNumTreesVisibleOutsideGridStr =
        parseGrid exampleInput |> findNumTreesVisibleFromOutsideGrid |> Num.toStr

    numTreesVisibleOutsideGridStr : Str
    numTreesVisibleOutsideGridStr =
        parseGrid input |> findNumTreesVisibleFromOutsideGrid |> Num.toStr

    exampleHighestScenicScoreStr : Str
    exampleHighestScenicScoreStr =
        parseGrid exampleInput |> findHighestScenicScore |> Num.toStr

    highestScenicScoreStr : Str
    highestScenicScoreStr =
        parseGrid input |> findHighestScenicScore |> Num.toStr

    Stdout.line
        """
        trees visible from outside the grid: \(numTreesVisibleOutsideGridStr)
        example trees visible from outside the grid: \(exampleNumTreesVisibleOutsideGridStr)

        highest scenic score possible: \(highestScenicScoreStr)
        example highest scenic score possible: \(exampleHighestScenicScoreStr)
        """

parseGrid : Str -> List (List U8)
parseGrid = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map \line ->
        Str.graphemes line
        |> List.map \a ->
            Str.toU8 a |> Result.withDefault 0

findNumTreesVisibleFromOutsideGrid : List (List U8) -> Nat
findNumTreesVisibleFromOutsideGrid = \grid ->
    boolGrid =
        List.mapWithIndex grid \line, vIndex ->
            List.mapWithIndex line \_, hIndex ->
                isTreeVisible vIndex hIndex grid

    List.walk boolGrid 0 \state, list ->
        List.walk list state \state2, bool ->
            if bool then state2 + 1 else state2

isTreeVisible : Nat, Nat, List (List U8) -> Bool
isTreeVisible = \vIndex, hIndex, grid ->
    vLine = List.map grid \line -> List.get line hIndex |> Result.withDefault 0
    hLine = List.get grid vIndex |> Result.withDefault []

    isTreeVisibleInLine vIndex vLine
    || isTreeVisibleInLine hIndex hLine

isTreeVisibleInLine : Nat, List U8 -> Bool
isTreeVisibleInLine = \index, line ->
    { before, others } = List.split line index
    after = List.dropFirst others
    tree = List.first others |> Result.withDefault 0

    List.isEmpty before
    || List.isEmpty after
    || List.all before \a -> a < tree
    || List.all after \a -> a < tree

findHighestScenicScore : List (List U8) -> Nat
findHighestScenicScore = \grid ->
    scoreGrid =
        List.mapWithIndex grid \line, vIndex ->
            List.mapWithIndex line \_, hIndex ->
                findHighestScenicScoreForTree vIndex hIndex grid

    List.walk scoreGrid 0 \state, list ->
        List.walk list state \state2, score ->
            List.max [state, state2, score]
            |> Result.withDefault score

findHighestScenicScoreForTree : Nat, Nat, List (List U8) -> Nat
findHighestScenicScoreForTree = \vIndex, hIndex, grid ->
    vLine = List.map grid \line -> List.get line hIndex |> Result.withDefault 0
    hLine = List.get grid vIndex |> Result.withDefault []

    getScenicScoreForLine vIndex vLine
    * getScenicScoreForLine hIndex hLine

getScenicScoreForLine : Nat, List U8 -> Nat
getScenicScoreForLine = \index, line ->
    { before, others } = List.split line index
    after = List.dropFirst others
    tree = List.first others |> Result.withDefault 0

    a = List.walkUntil (List.reverse before) 0 \state, visibleTree ->
        if visibleTree >= tree then
            Break (state + 1)
        else
            Continue (state + 1)

    b = List.walkUntil after 0 \state, visibleTree ->
        if visibleTree >= tree then
            Break (state + 1)
        else
            Continue (state + 1)

    a * b

# Tests
exampleInput =
    """
    30373
    25512
    65332
    33549
    35390
    """

expect findNumTreesVisibleFromOutsideGrid (parseGrid exampleInput) == 21
expect findHighestScenicScore (parseGrid exampleInput) == 8
