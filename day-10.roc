app "day-10"
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
    input <- File.readUtf8 (Path.fromStr "day-10-input.txt")
        |> Task.onFail \_ -> crash "Could not read file."
        |> Task.await

    outputExampleStr = parseInstructions exampleInput |> sumSixSignalStrengths |> List.sum |> Num.toStr
    outputPart1Str = parseInstructions input |> sumSixSignalStrengths |> List.sum |> Num.toStr

    view =
        parseInstructions input
        |> render
        |> Str.graphemes
        |> List.walk (T 0 "") \T index str, grapheme ->
            if index % 40 == 0 then
                T (index + 1) (Str.concat str (Str.concat "\n" grapheme))
            else
                T (index + 1) (Str.concat str grapheme)
        |> \T _ str -> str

    Stdout.line
        """
        six signal strengths sum: \(outputPart1Str)
        example: \(outputExampleStr)

        output2 \(view)
        """

Instruction : [AddX I64, NoOp]

parseInstructions : Str -> List Instruction
parseInstructions = \input ->
    input
    |> Str.split "\n"
    |> List.keepOks parseInstruction

parseInstruction : Str -> Result Instruction [InvalidInstruction]
parseInstruction = \str ->
    if str == "noop" then
        Ok NoOp
    else
        when Str.split str " " is
            ["addx", numStr] -> Str.toI64 numStr |> Result.map AddX
            _ -> Err InvalidInstruction

sumSixSignalStrengths : List Instruction -> List I64
sumSixSignalStrengths = \instructions ->
    instructions
    |> List.walk { cycle: 1, registerX: 1, signalStr: [] } \state, instruction ->
        runInstruction state instruction
    |> .signalStr

runInstruction :
    {
        cycle : U64,
        registerX : I64,
        signalStr : List I64,
    },
    Instruction
    ->
    {
        cycle : U64,
        registerX : I64,
        signalStr : List I64,
    }
runInstruction = \{ cycle, registerX, signalStr }, instruction ->
    sigStrCheckpoint = [20, 60, 100, 140, 180, 220]

    when instruction is
        AddX num ->
            if List.any sigStrCheckpoint \a -> a == cycle then
                {
                    cycle: cycle + 2,
                    signalStr: List.append signalStr (Num.toI64 cycle * registerX),
                    registerX: registerX + num,
                }
            else if List.any sigStrCheckpoint \a -> a == cycle + 1 then
                {
                    cycle: cycle + 2,
                    signalStr: List.append signalStr ((Num.toI64 cycle + 1) * registerX),
                    registerX: registerX + num,
                }
            else
                {
                    cycle: cycle + 2,
                    signalStr: signalStr,
                    registerX: registerX + num,
                }

        NoOp ->
            if List.any sigStrCheckpoint (\a -> a == cycle) then
                {
                    cycle: cycle + 1,
                    signalStr: List.append signalStr (Num.toI64 cycle * registerX),
                    registerX,
                }
            else
                {
                    cycle: cycle + 1,
                    signalStr: signalStr,
                    registerX,
                }

render : List Instruction -> Str
render = \instructions ->
    instructions
    |> List.walk { cycle: 1, registerX: 1, view: "" } \state, instruction ->
        renderInstruction state instruction
    |> .view

renderInstruction :
    {
        cycle : U64,
        registerX : I64,
        view : Str,
    },
    Instruction
    ->
    {
        cycle : U64,
        registerX : I64,
        view : Str,
    }
renderInstruction = \{ cycle, registerX, view }, instruction ->
    when instruction is
        AddX num ->
            a = renderPixel { cycle: cycle, registerX: registerX }
            b = renderPixel { cycle: cycle + 1, registerX: registerX }

            {
                cycle: cycle + 2,
                registerX: registerX + num,
                view: Str.concat view (Str.concat a b),
            }

        NoOp ->
            a = renderPixel { cycle: cycle, registerX: registerX }

            {
                cycle: cycle + 1,
                registerX,
                view: Str.concat view a,
            }

renderPixel : { cycle : U64, registerX : I64 } -> Str
renderPixel = \{ cycle, registerX } ->
    cycleX = Num.toI64 cycle % 40

    if cycleX == registerX || cycleX == registerX + 1 || cycleX == registerX + 2 then
        "#"
    else
        " "

expect (exampleInput |> parseInstructions |> sumSixSignalStrengths |> List.sum) == 13140

exampleInput =
    """
    addx 15
    addx -11
    addx 6
    addx -3
    addx 5
    addx -1
    addx -8
    addx 13
    addx 4
    noop
    addx -1
    addx 5
    addx -1
    addx 5
    addx -1
    addx 5
    addx -1
    addx 5
    addx -1
    addx -35
    addx 1
    addx 24
    addx -19
    addx 1
    addx 16
    addx -11
    noop
    noop
    addx 21
    addx -15
    noop
    noop
    addx -3
    addx 9
    addx 1
    addx -3
    addx 8
    addx 1
    addx 5
    noop
    noop
    noop
    noop
    noop
    addx -36
    noop
    addx 1
    addx 7
    noop
    noop
    noop
    addx 2
    addx 6
    noop
    noop
    noop
    noop
    noop
    addx 1
    noop
    noop
    addx 7
    addx 1
    noop
    addx -13
    addx 13
    addx 7
    noop
    addx 1
    addx -33
    noop
    noop
    noop
    addx 2
    noop
    noop
    noop
    addx 8
    noop
    addx -1
    addx 2
    addx 1
    noop
    addx 17
    addx -9
    addx 1
    addx 1
    addx -3
    addx 11
    noop
    noop
    addx 1
    noop
    addx 1
    noop
    noop
    addx -13
    addx -19
    addx 1
    addx 3
    addx 26
    addx -30
    addx 12
    addx -1
    addx 3
    addx 1
    noop
    noop
    noop
    addx -9
    addx 18
    addx 1
    addx 2
    noop
    noop
    addx 9
    noop
    noop
    noop
    addx -1
    addx 2
    addx -37
    addx 1
    addx 3
    noop
    addx 15
    addx -21
    addx 22
    addx -6
    addx 1
    noop
    addx 2
    addx 1
    noop
    addx -10
    noop
    noop
    addx 20
    addx 1
    addx 2
    addx 2
    addx -6
    addx -11
    noop
    noop
    noop
    """
