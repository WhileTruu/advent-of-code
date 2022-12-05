app "day-5"
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
    input <- File.readUtf8 (Path.fromStr "day-5-input.txt")
        |> Task.onFail \_ -> crash "Could not read file."
        |> Task.await

    stacksOfCrates = [
        "TRDHQNPB",
        "VTJBGW",
        "QMVSDHRN",
        "CMNZP",
        "BZD",
        "ZWCV",
        "SLQVCNZG",
        "VNDMJGL",
        "GCZFMPT",
    ]

    movements =
        input
        |> Str.split "\n\n"
        |> List.get 1
        |> Result.withDefault ""
        |> Str.split "\n"
        |> List.map parseMovement

    part1 =
     movements
        |> List.walk ( (Ok stacksOfCrates))
            (\(currentStacksOfCrates), movement ->
                Result.try currentStacksOfCrates \a -> moveCrates a movement

            )
             |> Result.withDefault stacksOfCrates
    part2 =
     movements
        |> List.walk ((Ok stacksOfCrates))
            (\(currentStacksOfCrates), movement ->
                Result.try currentStacksOfCrates \a -> moveCratesPart2 a movement
            )
             |> Result.withDefault stacksOfCrates

    outputPart1 =
        part1
        |> List.map \a ->
            a
            |> Str.graphemes
            |> List.takeFirst 1
            |> Str.joinWith ""
        |> Str.joinWith ""
    outputPart2 =
        part2
        |> List.map \a ->
            a
            |> Str.graphemes
            |> List.takeFirst 1
            |> Str.joinWith ""
        |> Str.joinWith ""

    Stdout.line "\(outputPart1) \(outputPart2)"

moveCrates : List Str, { from : Nat, to : Nat, amount : Nat } -> Result (List Str) [OutOfBounds]
moveCrates = \stacksOfCrates, movement ->
    cratesFrom <- Result.try (List.get stacksOfCrates (movement.from - 1))
    cratesTo <- Result.try (List.get stacksOfCrates (movement.to - 1))

    crates : Str
    crates =
        cratesFrom
        |> Str.graphemes
        |> List.takeFirst movement.amount
        |> List.reverse
        |> Str.joinWith ""

    newCratesFrom : Str
    newCratesFrom =
        cratesFrom
        |> Str.graphemes
        |> List.drop movement.amount
        |> Str.joinWith ""

    newCratesTo =
        "\(crates)\(cratesTo)"

    stacksOfCrates
    |> List.set (movement.from - 1) newCratesFrom
    |> List.set (movement.to - 1) newCratesTo
    |> Ok

moveCratesPart2 : List Str, { from : Nat, to : Nat, amount : Nat } -> Result (List Str) [OutOfBounds]
moveCratesPart2 = \stacksOfCrates, movement ->
    cratesFrom <- Result.try (List.get stacksOfCrates (movement.from - 1))
    cratesTo <- Result.try (List.get stacksOfCrates (movement.to - 1))

    crates : Str
    crates =
        cratesFrom
        |> Str.graphemes
        |> List.takeFirst movement.amount
        |> Str.joinWith ""

    newCratesFrom : Str
    newCratesFrom =
        cratesFrom
        |> Str.graphemes
        |> List.drop movement.amount
        |> Str.joinWith ""

    newCratesTo =
        "\(crates)\(cratesTo)"

    stacksOfCrates
    |> List.set (movement.from - 1) newCratesFrom
    |> List.set (movement.to - 1) newCratesTo
    |> Ok

parseMovement : Str -> { from : Nat, to : Nat, amount : Nat }
parseMovement = \str ->
    str
    |> Str.replaceEach "move " ""
    |> Result.withDefault ""
    |> Str.replaceEach " from " ";"
    |> Result.withDefault ""
    |> Str.replaceEach " to " ";"
    |> Result.withDefault ""
    |> Str.split ";"
    |> \list ->
        when list is
            [amountStr, fromStr, toStr] ->
                amount <- Result.try (Str.toNat amountStr)
                from <- Result.try (Str.toNat fromStr)
                to <- Result.try (Str.toNat toStr)

                Ok { from: from, to: to, amount: amount }

            _ ->
                Err InvalidAmountOfItemsInList
    |> Result.withDefault { from: 0, to: 0, amount: 0 }

