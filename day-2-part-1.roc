app "day-2-part-1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [
        pf.Stdout,
        pf.File,
        pf.Task.{ await, Task },
        pf.Path.{ Path },
        Utils.Result,
    ]
    provides [main] to pf

main : Task {} *
main =
    input <- File.readUtf8 (Path.fromStr "day-2-input.txt")
        |> Task.onFail \_ -> Task.succeed "Could not read file."
        |> Task.await

    output =
        Str.trim input
        |> Str.split "\n"
        |> List.keepOks \str ->
            when Str.split str " " is
                [x1, x2] -> Utils.Result.map2 (parseShape x1) (parseShape x2) Round
                _ -> Err WeirdAmountOfElementsInRoundList
        |> List.map scoreRound
        |> List.sum

    Stdout.line (Num.toStr output)

Shape : [Rock, Paper, Scissors]

parseShape : Str -> Result Shape [UnknownStr]
parseShape = \str -> when str is
        "A" | "X" -> Ok Rock
        "B" | "Y" -> Ok Paper
        "C" | "Z" -> Ok Scissors
        _ -> Err UnknownStr

scoreRound : [Round Shape Shape] -> I64
scoreRound = \Round a b ->
    when b is
        Rock ->
            when a is
                Rock -> 4
                Paper -> 1
                Scissors -> 7

        Paper ->
            when a is
                Rock -> 8
                Paper -> 5
                Scissors -> 2

        Scissors ->
            when a is
                Rock -> 3
                Paper -> 9
                Scissors -> 6
