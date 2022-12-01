app "day-1-part-1"
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
    input <- File.readUtf8 (Path.fromStr "day-1-input.txt")
        |> Task.onFail \_ -> Task.succeed "Could not read file."
        |> Task.await

    output =
        input
        |> Str.trim
        |> Str.split "\n\n"
        |> List.map (\a -> List.keepOks (Str.split a "\n") Str.toU32)
        |> List.map (List.sum)
        |> List.max
        |> Result.withDefault 0

    Stdout.line (Num.toStr output)
