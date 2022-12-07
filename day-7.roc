app "day-7"
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
    input <- File.readUtf8 (Path.fromStr "day-7-input.txt")
        |> Task.onFail \_ -> crash "Could not read file."
        |> Task.await

    outputPart1 =
        Num.toStr (getAnswer input)

    terminalCmds = inputToTerminalCmds exampleInput

    terminalCmdsStr : Str
    terminalCmdsStr =
        List.map terminalCmds terminalCmdToString |> Str.joinWith "\n"

    Stdout.line "\(outputPart1) \(terminalCmdsStr)"

getAnswer : Str -> Nat
getAnswer = \str -> 0

FileOrDir : [File Nat, Dir Str (Dict Str FileOrDir)]

inputToTree : Str -> Dict Str (List FileOrDir)

TerminalCmd : [
    TerminalCmdIn Str,
    TerminalCmdOut,
    TerminalCmdRoot,
    TerminalCmdLs (List FileOrDirName),
]

inputToTerminalCmds : Str -> List TerminalCmd
inputToTerminalCmds = \input ->
    input
    |> Str.split "$ "
    |> List.map Str.trim
    |> List.walk [] \state, line ->
        if line == "cd /" then
            List.append state TerminalCmdRoot
        else if Str.startsWith line "ls" then
            Str.replaceFirst line "ls" ""
            |> Result.withDefault line
            |> Str.split "\n"
            |> List.keepOks parseFileOrDirName
            |> \a -> List.append state (TerminalCmdLs a)
        else if Str.startsWith line "cd " then
            Str.replaceFirst line "cd " ""
            |> Result.withDefault line
            |> TerminalCmdIn
            |> \a -> List.append state a
        else
            state

terminalCmdToString : TerminalCmd -> Str
terminalCmdToString = \cmd ->
    when cmd is
        TerminalCmdIn dirname -> Str.concat "$ cd " dirname
        TerminalCmdOut -> "$ cd .."
        TerminalCmdRoot -> "$ cd /"
        TerminalCmdLs dirOrFileNames ->
            Str.concat
                "$ ls\n"
                (
                    Str.joinWith
                        (List.map dirOrFileNames fileOrDirNameToStr)
                        "\n"
                )

FileOrDirName : [File Str Nat, Dir Str]

fileOrDirNameToStr : FileOrDirName -> Str
fileOrDirNameToStr = \a ->
    when a is
        File name size ->
            name
            |> Str.concat " "
            |> Str.concat (Num.toStr size)

        Dir name ->
            Str.concat "dir " name

parseFileOrDirName : Str -> Result FileOrDirName {}
parseFileOrDirName = \str ->
    if Str.startsWith str "dir " then
        Str.replaceFirst str "dir " ""
        |> Result.withDefault str
        |> Dir
        |> Ok
    else
        Str.split str " "
        |> \a ->
            when a is
                [size, name] ->
                    Str.toNat size
                    |> Result.withDefault 0
                    |> \a1 -> File name a1
                    |> Ok

                _ ->
                    Err {}

exampleInput =
    """
    $ cd /
    $ ls
    dir a
    14848514 b.txt
    8504156 c.dat
    dir d
    $ cd a
    $ ls
    dir e
    29116 f
    2557 g
    62596 h.lst
    $ cd e
    $ ls
    584 i
    $ cd ..
    $ cd ..
    $ cd d
    $ ls
    4060174 j
    8033020 d.log
    5626152 d.ext
    7214296 k
    """

expect getAnswer exampleInput == 95437
