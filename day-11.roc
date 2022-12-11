app "day-11"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
    ]
    provides [main] to pf

main : Task {} *
main =
    inspectCountsStr =
        runRounds input 10000
        |> Dict.walk "" \state, k, v ->
            kStr = Num.toStr k
            vStr = Num.toStr v

            "\(state)Monkey \(kStr) inspected items \(vStr) times.\n"

    lvl =
        levelOfMonkeyBusiness input 10000
        |> Num.toStr

    Stdout.line
        """
        \(inspectCountsStr)
        level of monkey business: \(lvl)
        """

Monkey : {
    items : List I64,
    operation : I64 -> I64,
    test : I64 -> Nat,
    mod : I64,
}

input : List Monkey
input = [
    monkey0,
    monkey1,
    monkey2,
    monkey3,
    monkey4,
    monkey5,
    monkey6,
    monkey7,
]

monkey0 : Monkey
monkey0 = {
    items: [96, 60, 68, 91, 83, 57, 85],
    operation: \a -> a * 2,
    test: \a -> if a % 17 == 0 then 2 else 5,
    mod: 17,
}

monkey1 : Monkey
monkey1 = {
    items: [75, 78, 68, 81, 73, 99],
    operation: \a -> a + 3,
    test: \a -> if a % 13 == 0 then 7 else 4,
    mod: 13,
}

monkey2 : Monkey
monkey2 = {
    items: [69, 86, 67, 55, 96, 69, 94, 85],
    operation: \a -> a + 6,
    test: \a -> if a % 19 == 0 then 6 else 5,
    mod: 19,
}

monkey3 : Monkey
monkey3 = {
    items: [88, 75, 74, 98, 80],
    operation: \a -> a + 5,
    test: \a -> if a % 7 == 0 then 7 else 1,
    mod: 7,
}

monkey4 : Monkey
monkey4 = {
    items: [82],
    operation: \a -> a + 8,
    test: \a -> if a % 11 == 0 then 0 else 2,
    mod: 11,
}

monkey5 : Monkey
monkey5 = {
    items: [72, 92, 92],
    operation: \a -> a * 5,
    test: \a -> if a % 3 == 0 then 6 else 3,
    mod: 3,
}

monkey6 : Monkey
monkey6 = {
    items: [74, 61],
    operation: \a -> a * a,
    test: \a -> if a % 2 == 0 then 3 else 1,
    mod: 2,
}

monkey7 : Monkey
monkey7 = {
    items: [76, 86, 83, 55],
    operation: \a -> a + 4,
    test: \a -> if a % 5 == 0 then 4 else 0,
    mod: 5,
}

exampleInput : List Monkey
exampleInput = [
    exampleMonkey0,
    exampleMonkey1,
    exampleMonkey2,
    exampleMonkey3,
]

exampleMonkey0 : Monkey
exampleMonkey0 = {
    items: [79, 98],
    operation: \a -> a * 19,
    test: \a -> if a % 23 == 0 then 2 else 3,
    mod: 23,
}

exampleMonkey1 : Monkey
exampleMonkey1 = {
    items: [54, 65, 75, 74],
    operation: \a -> a + 6,
    test: \a -> if a % 19 == 0 then 2 else 0,
    mod: 19,
}

exampleMonkey2 : Monkey
exampleMonkey2 = {
    items: [79, 60, 97],
    operation: \a -> a * a,
    test: \a -> if a % 13 == 0 then 1 else 3,
    mod: 13,
}

exampleMonkey3 : Monkey
exampleMonkey3 = {
    items: [74],
    operation: \a -> a + 3,
    test: \a -> if a % 17 == 0 then 0 else 1,
    mod: 17,
}

levelOfMonkeyBusiness : List Monkey, Nat -> I64
levelOfMonkeyBusiness = \monkeys, rounds ->
    runRounds monkeys rounds
    |> Dict.toList
    |> List.map \T k v -> v
    |> List.sortDesc
    |> List.takeFirst 2
    |> List.product

runRounds : List Monkey, Nat -> Dict Nat I64
runRounds = \monkeys, numRounds ->
    runRoundsHelp (T monkeys Dict.empty) numRounds

runRoundsHelp : [T (List Monkey) (Dict Nat I64)], Nat -> Dict Nat I64
runRoundsHelp = \T monkeys inspectCounts, numRounds ->
    if numRounds == 0 then
        inspectCounts
    else
        monkeysAndInspectCounts = runRoundHelp (T monkeys inspectCounts) 0

        runRoundsHelp monkeysAndInspectCounts (numRounds - 1)

runRoundHelp : [T (List Monkey) (Dict Nat I64)], Nat -> [T (List Monkey) (Dict Nat I64)]
runRoundHelp = \T monkeys inspectCounts, index ->
    when List.get monkeys index is
        Ok monkey ->
            newMonkeys =
                monkey.items
                |> List.walk monkeys \state, item ->
                    inspectAndThrow monkey state item
                |> \a -> List.set a index { monkey & items: [] }

            newInspectCounts =
                Dict.update inspectCounts index \a ->
                    when a is
                        Missing -> Present (Num.toI64 (List.len monkey.items))
                        Present value -> Present (Num.toI64 (List.len monkey.items) + value)

            runRoundHelp (T newMonkeys newInspectCounts) (index + 1)

        Err _ ->
            T monkeys inspectCounts


inspectAndThrow : Monkey, List Monkey, I64 -> List Monkey
inspectAndThrow = \{ operation, test, mod }, monkeys, item ->
    inspectWorry = operation item
    boredWorry = inspectWorry % (List.product (List.map monkeys .mod))
    monkeyIndex = test boredWorry

    monkey = when List.get monkeys monkeyIndex is
        Ok a -> a
        _ -> crash (listGetCrashDebugStr monkeys monkeyIndex boredWorry)

    List.set monkeys monkeyIndex { monkey & items: List.append monkey.items boredWorry }

listGetCrashDebugStr : List Monkey, Nat, I64 -> Str
listGetCrashDebugStr = \monkeys, index, itemWorry ->
    items = whatItemsAreMonkeysHolding monkeys
    indexStr = Num.toStr index
    itemWorryStr = Num.toStr itemWorry

    "Could not get monkey at index \(indexStr), item: \(itemWorryStr), \n\(items)"

whatItemsAreMonkeysHolding : List Monkey -> Str
whatItemsAreMonkeysHolding = \monkeys ->
    monkeys
    |> List.mapWithIndex
        (\monkey, index ->
            List.map monkey.items Num.toStr
            |> Str.joinWith ", "
            |> \a ->
                indexStr = Num.toStr index

                "Monkey \(indexStr): \(a)"
        )
    |> Str.joinWith "\n"

expect levelOfMonkeyBusiness exampleInput 10000 == 2713310158
