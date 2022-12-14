app "day-13"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [
        pf.Stdout,
        pf.File,
        pf.Task.{ await, Task },
        pf.Path.{ Path },
        Parser,
        Parser.Str,
        Utils.Result,
    ]
    provides [main] to pf

main : Task {} *
main =
    input <- File.readUtf8 (Path.fromStr "day-13-input.txt")
        |> Task.onFail \_ -> crash "Could not read file."
        |> Task.await

    exampleStuff = pairsFromInput input
        |> List.walk (T [] 1) \T state index, Pair left right ->
            order = pairOrder left right
            if order == LT || order == EQ then
                T (List.append state index) (index + 1)
            else
                T state (index + 1)
        |> \T state _ -> state
        |> List.sum
        |> Num.toStr

    sortedExampleStuff =
        packetsFromInput exampleInput
        # |> List.append (PacketList [PacketList [PacketInt 2]])
        # |> List.append (PacketList [PacketList [PacketInt 6]])
        |> List.sortWith \a, b -> pairOrderHelp a b [] []
        |> List.map printPacketData
        |> Str.joinWith "\n"

    Stdout.line
        """
        \(exampleStuff)
        \(sortedExampleStuff)
        """


pairOrder : PacketData, PacketData -> [GT, LT, EQ]
pairOrder = \a, b ->
    #dbg a
    when Pair a b is
        Pair (PacketList left) (PacketList right) ->
            if List.isEmpty left && List.isEmpty right then
                EQ
            else if List.isEmpty left then
                LT
            else if List.isEmpty right then
                GT
            else
                leftFirst = List.first left |> resultDefaultCrash "yolo"
                rightFirst = List.first right |> resultDefaultCrash "yolo"
                othersLeft = List.drop left 1
                othersRight = List.drop right 1

                when pairOrder leftFirst rightFirst is
                    GT -> GT
                    LT -> LT
                    EQ -> pairOrder (PacketList othersLeft) (PacketList othersRight)

        Pair (PacketInt left) (PacketInt right) -> Num.compare left right
        Pair (PacketInt left) (PacketList right) -> pairOrder (PacketList [PacketInt left]) (PacketList right)
        Pair (PacketList left) (PacketInt right) -> pairOrder (PacketList left) (PacketList [PacketInt right])

pairOrderHelp : PacketData, PacketData, List PacketData, List PacketData -> [GT, LT, EQ]
pairOrderHelp = \a, b, aOthers, bOthers ->
    #dbg a
    when Pair a b is
        Pair (PacketList left) (PacketList right) ->
            if List.isEmpty left && List.isEmpty right then
                pairOrderHelp (PacketList aOthers) (PacketList bOthers) [] []
            else if List.isEmpty left then
                LT
            else if List.isEmpty right then
                GT
            else
                leftFirst = List.first left |> resultDefaultCrash "yolo"
                rightFirst = List.first right |> resultDefaultCrash "yolo"
                othersLeft = List.drop left 1
                othersRight = List.drop right 1

                pairOrderHelp leftFirst rightFirst aOthers bOthers

        Pair (PacketInt left) (PacketInt right) -> Num.compare left right
        Pair (PacketInt left) (PacketList right) -> pairOrderHelp (PacketList [PacketInt left]) (PacketList right) [] []
        Pair (PacketList left) (PacketInt right) -> pairOrderHelp (PacketList left) (PacketList [PacketInt right]) [] []

pairsFromInput : Str -> List [Pair PacketData PacketData]
pairsFromInput = \input ->
    input
        |> Str.split "\n\n"
        |> List.keepOks \a ->
            when Str.split a "\n" is
                [first, second] ->
                    Utils.Result.map2
                        (Parser.parse packetDataParser (Str.toUtf8 first) List.isEmpty)
                        (Parser.parse packetDataParser (Str.toUtf8 second) List.isEmpty)
                        Pair
                _ -> Err Yolo

PacketData : [PacketList (List PacketData), PacketInt Nat]

printPacketData : PacketData -> Str
printPacketData = \packetData ->
    when packetData is
        PacketList packetList ->
            packetList
            |> List.map printPacketData
            |> Str.joinWith ","
            |> \s -> "[\(s)]"
        PacketInt n -> Num.toStr n

packetDataParser : Parser.Parser (List U8) PacketData
packetDataParser =
    Parser.oneOf [
        Parser.lazy \_ -> packetDataParser,
        Parser.map parseDigits PacketInt,
    ]
    |> Parser.sepBy (Parser.Str.scalar ',')
    |> Parser.between (Parser.Str.scalar '[') (Parser.Str.scalar ']')
    |> Parser.map PacketList

resultDefaultCrash = \res, message ->
    when res is
        Ok value -> value
        Err _ -> crash message

parseDigits : Parser.Parser (List U8) (Int *)
parseDigits =
    Parser.oneOrMore Parser.Str.digit
    |> Parser.map \digitsList ->
        digitsList
        |> List.map \a -> Num.intCast (a - 48)
        |> List.walk 0 \sum, digitVal -> 10 * sum + digitVal


packetsFromInput : Str -> List PacketData
packetsFromInput = \input ->
    input
        |> Str.split "\n"
        |> List.keepOks \a -> Parser.parse packetDataParser (Str.toUtf8 a) List.isEmpty

exampleInput =
    """
    [1,1,3,1,1]
    [1,1,5,1,1]

    [[1],[2,3,4]]
    [[1],4]

    [9]
    [[8,7,6]]

    [[4,4],4,4]
    [[4,4],4,4,4]

    [7,7,7,7]
    [7,7,7]

    []
    [3]

    [[[]]]
    [[]]

    [1,[2,[3,[4,[5,6,7]]]],8,9]
    [1,[2,[3,[4,[5,6,0]]]],8,9]
    """

expect
    Parser.parse packetDataParser (Str.toUtf8 "[[1],[2,3,4]]") List.isEmpty
    ==
    Ok
        (
            PacketList [
                PacketList [PacketInt 1],
                PacketList [PacketInt 2, PacketInt 3, PacketInt 4],
            ]
        )

expect
    pairsFromInput exampleInput
    |> List.map \Pair a b ->
        aStr = printPacketData a
        bStr = printPacketData b
        "\(aStr)\n\(bStr)"
    |> Str.joinWith "\n\n"
    |> \a -> a == exampleInput



expect
    packetsFromInput exampleInput
    |> List.append (PacketList [PacketList [PacketInt 2]])
    |> List.append (PacketList [PacketList [PacketInt 6]])
    |> List.sortWith \a, b -> pairOrderHelp a b [] []
    |> List.map printPacketData
    |> Str.joinWith "\n"
    |> \a -> a ==
        """
        []
        [[]]
        [[[]]]
        [1,1,3,1,1]
        [1,1,5,1,1]
        [[1],[2,3,4]]
        [1,[2,[3,[4,[5,6,0]]]],8,9]
        [1,[2,[3,[4,[5,6,7]]]],8,9]
        [[1],4]
        [[2]]
        [3]
        [[4,4],4,4]
        [[4,4],4,4,4]
        [[6]]
        [7,7,7]
        [7,7,7,7]
        [[8,7,6]]
        [9]
        """