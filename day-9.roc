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

    outputPart1 = findPositionsTailOfRopeVisits input

    outputPart1Str = outputPart1 |> Set.len |> Num.toStr
    locsStr = outputPart1 |> Set.toList |> List.map locToStr |> Str.joinWith ", "

    Stdout.line
        """
        \(outputPart1Str)
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

doMotion : Loc, Loc, Motion -> { head : Loc, tail : Loc }
doMotion = \head, tail, motion ->
    newHead = doMotionForLoc head motion

    newTail = moveTail { head: newHead, tail } motion

    { head: newHead, tail: newTail }

moveTail : { head: Loc, tail: Loc }, Motion -> Loc
moveTail = \{ head, tail }, motion ->
    if Num.abs (tail.v - head.v) == 2 then
        if tail.h != head.h then
            a = doMotionForLoc tail motion

            { a & h: head.h }
        else
            doMotionForLoc tail motion
    else if Num.abs (tail.h - head.h) == 2 then
        if tail.v != head.v then
            a = doMotionForLoc tail motion

            { a & v: head.v }
        else
            doMotionForLoc tail motion
    else
        tail

findPositionsTailOfRopeVisits : Str -> Set Loc
findPositionsTailOfRopeVisits = \input ->
    motions = parseMotions input

    motions
    |> List.walk (T { v: 0, h: 0 } { v: 0, h: 0 } Set.empty) \T locH locT uniqueLocs, motion ->
        { head, tail } = doMotion locH locT motion

        T head tail (Set.insert uniqueLocs tail)
    |> \T _ _ list -> list

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

expect Set.len (findPositionsTailOfRopeVisits exampleInput) == 13
