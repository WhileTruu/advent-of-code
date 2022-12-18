app "day-16"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
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
    input <- File.readUtf8 (Path.fromStr "day-16-input.txt")
        |> Task.onFail \_ -> crash "Could not read file."
        |> Task.await
    xample =
        Dict.toList (parseScanOutput exampleInput)
        |> List.map \T a _ -> a
        |> Str.joinWith ", "

    Stdout.line
        """

        \(xample)
        
        """

parseScanOutput : Str -> Dict Str { flowRate : Nat, tunnels : Set Str }
parseScanOutput = \input ->
    input
    |> Str.split "\n"
    |> List.map parseScanOutputLine
    |> Dict.fromList

parseScanOutputLine : Str -> [T Str { flowRate : Nat, tunnels : Set Str }]
parseScanOutputLine = \input ->
    input
    |> Str.replaceFirst "Valve " ""
    |> Result.try \a -> Str.replaceFirst a " has flow rate=" ";"
    |> Result.map \a -> Str.replaceFirst a " tunnels lead to valves " "" |> Result.withDefault a
    |> Result.map \a -> Str.replaceFirst a " tunnel leads to valve " "" |> Result.withDefault a
    |> Result.map \a -> Str.split a ";"
    |> Result.try \a -> when a is
            [valve, flowRateStr, tunnelsStr] ->
                flowRate = flowRateStr |> Str.toNat |> Result.withDefault 0
                tunnels = tunnelsStr |> Str.split ", " |> Set.fromList

                Ok (T valve { flowRate, tunnels })

            _ -> Err CouldNotParseLine
    |> Utils.Result.withDefaultLazy \_ -> crash "Could not parse line."

exampleInput : Str
exampleInput =
    """
    Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
    Valve BB has flow rate=13; tunnels lead to valves CC, AA
    Valve CC has flow rate=2; tunnels lead to valves DD, BB
    Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
    Valve EE has flow rate=3; tunnels lead to valves FF, DD
    Valve FF has flow rate=0; tunnels lead to valves EE, GG
    Valve GG has flow rate=0; tunnels lead to valves FF, HH
    Valve HH has flow rate=22; tunnel leads to valve GG
    Valve II has flow rate=0; tunnels lead to valves AA, JJ
    Valve JJ has flow rate=21; tunnel leads to valve II
    """

expect
    Dict.toList (parseScanOutput exampleInput)
    == [
        T "AA" { flowRate: 0, tunnels: Set.fromList ["DD", "II", "BB"] },
        T "BB" { flowRate: 13, tunnels: Set.fromList ["CC", "AA"] },
        T "CC" { flowRate: 2, tunnels: Set.fromList ["DD", "BB"] },
        T "DD" { flowRate: 20, tunnels: Set.fromList ["CC", "AA", "EE"] },
        T "EE" { flowRate: 3, tunnels: Set.fromList ["FF", "DD"] },
        T "FF" { flowRate: 0, tunnels: Set.fromList ["EE", "GG"] },
        T "GG" { flowRate: 0, tunnels: Set.fromList ["FF", "HH"] },
        T "HH" { flowRate: 22, tunnels: Set.fromList ["GG"] },
        T "II" { flowRate: 0, tunnels: Set.fromList ["AA", "JJ"] },
        T "JJ" { flowRate: 21, tunnels: Set.fromList ["II"] },
    ]
