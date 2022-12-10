app "day-9"
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
    input <- File.readUtf8 (Path.fromStr "day-9-input.txt")
        |> Task.onFail \_ -> crash "Could not read file."
        |> Task.await

    outputPart1 = findPositionsTailOfRopeVisits input 1
    exampleOutputPart2 = findPositionsTailOfRopeVisits exampleInput2 9 |> Set.len |> Num.toStr

    outputPart1Str = outputPart1 |> Set.len |> Num.toStr
    locsStr = findPositionsTailOfRopeVisits exampleInput2 9 |> Set.toList |> List.map locToStr |> Str.joinWith ", "

    outputPart2Str = findPositionsTailOfRopeVisits input 9 |> Set.len |> Num.toStr

    Stdout.line
        """
        output part 1: \(outputPart1Str) (should be 6376)
        example output part 2: \(exampleOutputPart2) (should be 36)
        output part 2: \(outputPart2Str)

        \(locsStr)
        """

Motion : [Up, Right, Down, Left]

parseMotions : Str -> List Motion
parseMotions = \input ->
    input
    |> Str.split "\n"
    |> List.keepOks \line ->
        { before, after } <- Result.try (Str.splitFirst line " ")
        amount <- Result.try (Str.toNat after)
        motion <- Result.try (motionFromStr before)

        Ok (List.repeat motion amount)
    |> List.walk [] (\state, a -> List.concat state a)

motionFromStr : Str -> Result Motion [UnknownMotion Str]
motionFromStr = \motion ->
    when motion is
        "U" -> Ok Up
        "R" -> Ok Right
        "D" -> Ok Down
        "L" -> Ok Left
        garbage -> Err (UnknownMotion garbage)

Loc : { v : I32, h : I32 }

locToStr : Loc -> Str
locToStr = \loc ->
    vStr = Num.toStr loc.v
    hStr = Num.toStr loc.h

    "(\(vStr), \(hStr))"

doMotionForLoc : Loc, Motion -> Loc
doMotionForLoc = \loc, motion ->
    when motion is
        Up -> { loc & v: loc.v + 1 }
        Right -> { loc & h: loc.h + 1 }
        Down -> { loc & v: loc.v - 1 }
        Left -> { loc & h: loc.h - 1 }

doMotion : Loc, List Loc, Motion -> { head : Loc, tails : List Loc }
doMotion = \head, tails, motion ->
    newHead = doMotionForLoc head motion

    newTails =
        tails
        |> List.walk (T newHead []) \T a list, tail ->
            x = moveTail { head: a, tail }

            T x (List.append list x)
        |> \T _ a -> a

    { head: newHead, tails: newTails }

moveTail : { head : Loc, tail : Loc } -> Loc
moveTail = \{ head, tail } ->
    vDiff = (head.v - tail.v) // 2
    hDiff = (head.h - tail.h) // 2

    v =
        if Num.abs vDiff == 1 then
            tail.v + vDiff
        else if Num.abs hDiff == 1 then
            head.v
        else
            tail.v

    h =
        if Num.abs hDiff == 1 then
            tail.h + hDiff
        else if Num.abs vDiff == 1 then
            head.h
        else
            tail.h

    { v, h }

findPositionsTailOfRopeVisits : Str, Nat -> Set Loc
findPositionsTailOfRopeVisits = \input, numTails ->
    motions = parseMotions input
    startHead = { v: 0, h: 0 }
    startTails = List.repeat { v: 0, h: 0 } numTails

    motions
    |> List.walk (T startHead startTails Set.empty) \T head tails uniqueLocs, motion ->
        new = doMotion head tails motion

        newUniqueLocs =
            List.last new.tails
            |> Result.map \a -> Set.insert uniqueLocs a
            |> Result.withDefault uniqueLocs

        T new.head new.tails newUniqueLocs
    |> \T _ _ a -> a

# Tests
exampleInput =
    """
    R 4
    U 4
    L 3
    D 1
    R 4
    D 1
    L 5
    R 2
    """

exampleInput2 =
    """
    R 5
    U 8
    L 8
    D 3
    R 17
    D 10
    L 25
    U 20
    """

expect Set.len (findPositionsTailOfRopeVisits exampleInput 1) == 13
expect Set.len (findPositionsTailOfRopeVisits exampleInput 9) == 1
expect Set.len (findPositionsTailOfRopeVisits exampleInput2 9) == 36
