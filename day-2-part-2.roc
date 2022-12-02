app "day-2-part-2"
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
                [x1, x2] -> Utils.Result.map2 (parseShape x1) (parseRoundResult x2) Round
                _ -> Err WeirdAmountOfElementsInRoundList
        |> List.map scoreRound
        |> List.sum

    Stdout.line (Num.toStr output)

Shape : [Rock, Paper, Scissors]

parseShape : Str -> Result Shape [UnknownStr]
parseShape = \str -> when str is
        "A" -> Ok Rock
        "B" -> Ok Paper
        "C" -> Ok Scissors
        _ -> Err UnknownStr

RoundResult : [ Lose, Draw, Win ]

parseRoundResult : Str -> Result RoundResult [UnknownStr]
parseRoundResult = \str -> when str is
        "X" -> Ok Lose
        "Y" -> Ok Draw
        "Z" -> Ok Win
        _ -> Err UnknownStr

scoreRound : [Round Shape RoundResult] -> I64
scoreRound = \Round a b ->
    when b is
        Lose ->
            when a is
                Rock -> 3
                Paper -> 1
                Scissors -> 2

        Draw ->
            when a is
                Rock -> 4
                Paper -> 5
                Scissors -> 6

        Win ->
            when a is
                Rock -> 8
                Paper -> 9
                Scissors -> 7
