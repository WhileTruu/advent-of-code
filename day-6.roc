app "day-6"
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
    input <- File.readUtf8 (Path.fromStr "day-6-input.txt")
        |> Task.onFail \_ -> crash "Could not read file."
        |> Task.await

    outputPart1 =
        Num.toStr (firstPositionWhereNumberOfCharactersAreAllDifferent input 4)
    outputPart2 =
        Num.toStr (firstPositionWhereNumberOfCharactersAreAllDifferent input 14)
    Stdout.line "\(outputPart1) \(outputPart2)"

firstPositionWhereNumberOfCharactersAreAllDifferent : Str, Nat -> Nat
firstPositionWhereNumberOfCharactersAreAllDifferent = \input, reqNumDifferent ->
    input
    |> Str.graphemes
    |> List.walkUntil { index: 0, chars: [] } \{ index, chars }, char ->
        if
            !(List.any chars (\a -> a == char))
            && (List.len chars == reqNumDifferent - 1)
        then
            Break {
                index: index + 1,
                chars: List.append chars char,
            }
        else
            lastIndexOfChar =
                List.findLastIndex chars (\a -> a == char)

            newChars = when lastIndexOfChar is
                Ok i ->
                    List.split chars (i + 1) |> .others

                Err _ ->
                    chars

            Continue {
                index: index + 1,
                chars: List.append newChars char,
            }
    |> .index

expect firstPositionWhereNumberOfCharactersAreAllDifferent "bvwbjplbgvbhsrlpgdmjqwftvncz" 4 == 5
expect firstPositionWhereNumberOfCharactersAreAllDifferent "nppdvjthqldpwncqszvftbrmjlhg" 4 == 6
expect firstPositionWhereNumberOfCharactersAreAllDifferent "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4 == 10
expect firstPositionWhereNumberOfCharactersAreAllDifferent "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4 == 11
