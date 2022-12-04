app "day-4"
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
    input <- File.readUtf8 (Path.fromStr "day-4-input.txt")
        |> Task.onFail \_ -> Task.succeed "Could not read file."
        |> Task.await

    assignmentPairs =
        Str.trim input
        |> Str.split "\n"
        |> List.map parseAssignmentPairRanges

    part1Output =
        assignmentPairs
        |> List.map \AssignmentPair a1 a2 -> doesRangeFullyContainAnother a1 a2
        |> List.countIf (\a -> a)
        |> Num.toStr

    part2Output =
        assignmentPairs
        |> List.map \AssignmentPair a1 a2 -> doRangesOverlap a1 a2
        |> List.countIf (\a -> a)
        |> Num.toStr

    Stdout.line ("\(part1Output) \(part2Output)")

parseAssignmentPairRanges : Str -> [AssignmentPair [Range U32 U32] [Range U32 U32]]
parseAssignmentPairRanges = \input ->
    input
    |> Str.split ","
    |> List.joinMap \a -> Str.split a "-"
    |> List.keepOks \a -> Str.toU32 a
    |> \list ->
        when list is
            [a1, a2, b1, b2] -> AssignmentPair (Range a1 a2) (Range b1 b2)
            _ -> AssignmentPair (Range 0 0) (Range 0 0)

doesRangeFullyContainAnother : [Range U32 U32], [Range U32 U32] -> Bool
doesRangeFullyContainAnother = \Range a1 a2, Range b1 b2 ->
    a1 >= b1 && a2 <= b2 || b1 >= a1 && b2 <= a2

doRangesOverlap : [Range U32 U32], [Range U32 U32] -> Bool
doRangesOverlap = \Range a1 a2, Range b1 b2 -> !(a1 > b2 || a2 < b1)
