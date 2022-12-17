app "day-15"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [
        pf.Stdout,
        pf.File,
        pf.Task.{ await, Task },
        pf.Path.{ Path },
        Utils.Result,
        Utils.Str,
        Utils.List,
    ]
    provides [main] to pf

main : Task {} *
main =
    input <- File.readUtf8 (Path.fromStr "day-15-input.txt")
        |> Task.onFail \_ -> crash "Could not read file."
        |> Task.await

    # lineNum = 10
    sensorsAndBeacons = parseInput input

    # x = howManyPositionsCanContainBeaconOnLine sensorsAndBeacons lineNum |> Num.toStr

    emptySpot = findEmptySpot sensorsAndBeacons 4000000
        |> Utils.Result.withDefaultLazy \_ -> crash "No empty spot."
        |> \a -> a.x * 4000000 + a.y
        |> Num.toStr

    Stdout.line
        """
        \(emptySpot)
        """

findEmptySpot : List { sensor : Pos, closestBeacon : Pos }, Nat -> Result Pos [NoEmptySpot]
findEmptySpot = \sensorsAndBeacons, max ->
    findEmptySpotHelp sensorsAndBeacons 0 max

findEmptySpotHelp : List { sensor : Pos, closestBeacon : Pos }, Nat, Nat -> Result Pos [NoEmptySpot]
findEmptySpotHelp = \sensorsAndBeacons, lineNum, max ->
    if lineNum < max then
        res =
            sensorsAndBeacons
            |> List.keepOks \a -> sensorRangeForLine a (Num.toI64 lineNum)
            |> Utils.List.bubbleSort \a, b -> Num.compare a.min b.min
            |> List.walk [] \state, a ->
                when state is
                    [] -> [a]
                    [.., last] ->
                        if last.max >= a.max then
                            state
                        else if a.min < last.max then
                            List.append state { a & min: last.max }
                        else
                            List.append state a
            |> List.walkUntil (Err []) \r, a ->
                when r is
                    Err ([]) -> Continue (Err [a])
                    Err ([.., last]) ->
                        if a.min > last.max then
                            Break (Ok { x: last.max, y: Num.toI64 lineNum })
                        else
                            Continue (Result.mapErr r \state -> List.append state a)
                    Ok x -> Break (Ok x)

        when res is
            Ok a -> Ok a
            Err a -> findEmptySpotHelp sensorsAndBeacons (lineNum + 1) max
    else
        Err NoEmptySpot

howManyPositionsCanContainBeaconOnLine : List { sensor : Pos, closestBeacon : Pos }, I64 -> I64
howManyPositionsCanContainBeaconOnLine = \sensorsAndBeacons, lineNum ->
    beaconsOnLine =
        sensorsAndBeacons
        |> List.keepIf \a -> a.closestBeacon.y == lineNum
        |> List.map \a -> a.closestBeacon
        |> Set.fromList
        |> Set.toList
        |> List.map .x

    ranges =
        sensorsAndBeacons
        |> List.keepOks \a -> sensorRangeForLine a lineNum
        |> Utils.List.bubbleSort \a, b -> Num.compare a.min b.min
        |> List.walk [] \state, a ->
            when state is
                [] -> [a]
                [.., last] ->
                    if last.max >= a.max then
                        state
                    else if a.min < last.max then
                        List.append state { a & min: last.max }
                    else
                        List.append state a

    ranges
    |> List.map \a ->
        beaconCountInRange =
            List.countIf beaconsOnLine (\bx -> bx >= a.min && bx <= a.max)

        Num.abs (a.max - a.min) - Num.toI64 beaconCountInRange
    |> List.sum

Pos : { x : I64, y : I64 }

sensorRangeForLine : { sensor : Pos, closestBeacon : Pos }, I64 -> Result { min : I64, max : I64 } [NoRangeForLine]
sensorRangeForLine = \{ sensor, closestBeacon }, line ->
    maxDistance = Num.abs (sensor.x - closestBeacon.x) + Num.abs (sensor.y - closestBeacon.y)

    lineSensorDist = Num.abs (sensor.y - line)
    distIntoLine = maxDistance - lineSensorDist

    if distIntoLine >= 0 then
        min = sensor.x - distIntoLine
        max = sensor.x + distIntoLine

        Ok { min, max: max + 1 }
    else
        Err NoRangeForLine

createGrid : List { sensor : Pos, closestBeacon : Pos }, Bounds -> List (List Str)
createGrid = \sensorAndBeaconList, bounds ->
    row : List Str
    row = List.repeat "." (Num.toNat (bounds.maxX - bounds.minX + 1))

    grid : List (List Str)
    grid =
        List.repeat row (Num.toNat (bounds.maxY - bounds.minY + 1))

    grid
    |> List.mapWithIndex \a, y ->
        sensorRanges = List.keepOks sensorAndBeaconList \sb -> sensorRangeForLine sb (Num.toI64 y + bounds.minY)

        List.mapWithIndex a \b, x ->
            pos = { x: bounds.minX + Num.toI64 x, y: bounds.minY + Num.toI64 y }

            if sensorAndBeaconList |> List.any \lv -> lv.sensor == pos then
                "S"
            else if sensorAndBeaconList |> List.any \lv -> lv.closestBeacon == pos then
                "B"
            else if List.any sensorRanges \asd -> asd.min <= pos.x && pos.x < asd.max then
                "+"
            else
                b

drawGrid : List (List Str), Bounds -> Str
drawGrid = \grid, bounds ->
    spacing : Nat
    spacing =
        a = Str.countGraphemes (Num.toStr bounds.maxY)
        b = Str.countGraphemes (Num.toStr bounds.minY)

        if a > b then a else b

    grid
    |> List.mapWithIndex \a, i ->
        indexStr = Num.toStr (bounds.minY + Num.toI64 i)

        lineStr = Str.joinWith a ""
        paddedIndexStr =
            indexStr
            |> Utils.Str.padLeft (spacing - Str.countGraphemes indexStr) " "

        "\(paddedIndexStr) \(lineStr)"
    |> List.prepend (drawHorizontalIndices bounds (spacing + 1))
    |> Str.joinWith "\n"

drawHorizontalIndices : Bounds, Nat -> Str
drawHorizontalIndices = \bounds, padAmount ->
    indices = List.range { start: At bounds.minX, end: At bounds.maxX }

    spacing : Nat
    spacing =
        a = Str.countGraphemes (Num.toStr bounds.maxX)
        b = Str.countGraphemes (Num.toStr bounds.minX)

        if a > b then a else b

    lines =
        indices
        |> List.map \a ->
            indexStr = Num.toStr a

            indexStr
            |> Str.graphemes
            |> \elems -> List.concat (List.repeat " " (spacing - Str.countGraphemes indexStr)) elems
        |> List.walk Dict.empty \state, a ->
            List.walk a (T state 0) \T dictState i, str ->
                newState =
                    Dict.update dictState i \value ->
                        when value is
                            Missing -> Present [str]
                            Present present -> Present (List.append present str)

                T newState (i + 1)
            |> \T newState _ -> newState
        |> Dict.toList
        |> List.map \T _ a ->
            List.mapWithIndex a \v, i -> if (Num.toI64 i - bounds.minX) % 5 == 0 then v else " "
            |> Str.joinWith ""
        |> List.map \a -> Utils.Str.padLeft a padAmount " "
        |> Str.joinWith "\n"

    lines

Bounds : { minX : I64, maxX : I64, minY : I64, maxY : I64 }

calcBounds : List { sensor : Pos, closestBeacon : Pos } -> Bounds
calcBounds = \sensorAndBeaconList ->
    positions : List Pos
    positions =
        sensorAndBeaconList
        |> List.walk [] \state, a -> state |> List.append a.sensor |> List.append a.closestBeacon

    boundsResult =
        minX <- positions |> List.map .x |> List.min |> Result.try
        minY <- positions |> List.map .y |> List.min |> Result.try
        maxX <- positions |> List.map .x |> List.max |> Result.try
        maxY <- positions |> List.map .y |> List.max |> Result.map

        { minX, maxX, minY, maxY }

    when boundsResult is
        Ok a -> a
        Err _ -> crash "Could not get min and max."

parseInput : Str -> List { sensor : Pos, closestBeacon : Pos }
parseInput = \str ->
    str
    |> Str.split "\n"
    |> List.map parseSensorAndClosestBeacon

parseSensorAndClosestBeacon : Str -> { sensor : Pos, closestBeacon : Pos }
parseSensorAndClosestBeacon = \value ->
    value
    |> Str.replaceFirst "Sensor at " ""
    |> Result.try \a -> Str.replaceFirst a " closest beacon is at " ""
    |> Result.map \a -> Str.split a ":"
    |> Result.try \a -> when a is
            [first, second] -> Ok { sensor: parsePos first, closestBeacon: parsePos second }
            _ -> Err NoPositions
    |> Utils.Result.withDefaultLazy \_ -> crash "Failed to parse sensor and closest beacon."

parsePos : Str -> Pos
parsePos = \value ->
    value
    |> Str.replaceEach "x=" ""
    |> Result.try \a -> Str.replaceEach a "y=" ""
    |> Result.map \a -> Str.split a ", "
    |> Result.try \a -> when a is
            [xStr, yStr] ->
                x <- Str.toI64 xStr |> Result.try
                y <- Str.toI64 yStr |> Result.map

                { x, y }

            _ ->
                Err InvalidPosComponents
    |> Utils.Result.withDefaultLazy \_ -> crash "Failed to parse pos."

exampleInput : Str
exampleInput =
    """
    Sensor at x=2, y=18: closest beacon is at x=-2, y=15
    Sensor at x=9, y=16: closest beacon is at x=10, y=16
    Sensor at x=13, y=2: closest beacon is at x=15, y=3
    Sensor at x=12, y=14: closest beacon is at x=10, y=16
    Sensor at x=10, y=20: closest beacon is at x=10, y=16
    Sensor at x=14, y=17: closest beacon is at x=10, y=16
    Sensor at x=8, y=7: closest beacon is at x=2, y=10
    Sensor at x=2, y=0: closest beacon is at x=2, y=10
    Sensor at x=0, y=11: closest beacon is at x=2, y=10
    Sensor at x=20, y=14: closest beacon is at x=25, y=17
    Sensor at x=17, y=20: closest beacon is at x=21, y=22
    Sensor at x=16, y=7: closest beacon is at x=15, y=3
    Sensor at x=14, y=3: closest beacon is at x=15, y=3
    Sensor at x=20, y=1: closest beacon is at x=15, y=3
    """

expect
    input = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
    expectedOutput = { sensor: { x: 2, y: 18 }, closestBeacon: { x: -2, y: 15 } }

    parseSensorAndClosestBeacon input == expectedOutput

expect
    input = "x=2, y=18"
    expectedOutput = { x: 2, y: 18 }

    parsePos input == expectedOutput

expect howManyPositionsCanContainBeaconOnLine (parseInput exampleInput) 10 == 26
expect howManyPositionsCanContainBeaconOnLine (parseInput exampleInput) 9 == 25
expect howManyPositionsCanContainBeaconOnLine (parseInput exampleInput) 11 == 28
