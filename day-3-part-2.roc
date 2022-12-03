app "day-3-part-2"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [
        pf.Stdout,
        pf.File,
        pf.Task.{ await, Task },
        pf.Path.{ Path },
    ]
    provides [main] to pf

# --- Part Two ---
#
# As you finish identifying the misplaced items, the Elves come to you with another
# issue.
#
# For safety, the Elves are divided into groups of three. Every Elf carries a badge
# that identifies their group. For efficiency, within each group of three Elves,
# the badge is the only item type carried by all three Elves. That is, if a group's
# badge is item type B, then all three Elves will have item type B somewhere in
# their rucksack, and at most two of the Elves will be carrying any other item type.
#
# The problem is that someone forgot to put this year's updated authenticity
# sticker on the badges. All of the badges need to be pulled out of the rucksacks
# so the new authenticity stickers can be attached.
#
# Additionally, nobody wrote down which item type corresponds to each group's badges.
# The only way to tell which item type is the right one is by finding the one item
# type that is common between all three Elves in each group.
#
# Every set of three lines in your list corresponds to a single group, but each
# group can have a different badge item type. So, in the above example, the first
# group's rucksacks are the first three lines:
#
#     vJrwpWtwJgWrhcsFMMfFFhFp
#     jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
#     PmmdzqPrVvPwwTWBwg
#
# And the second group's rucksacks are the next three lines:
#
#     wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
#     ttgJtRGJQctTZtZT
#     CrZsJsPPZsGzwwsLwLmpwMDw
#
# In the first group, the only item type that appears in all three rucksacks is
# lowercase r; this must be their badges. In the second group, their badge item
# type must be Z.
#
# Priorities for these items must still be found to organize the sticker attachment
# efforts: here, they are 18 (r) for the first group and 52 (Z) for the second group.
# The sum of these is 70.
#
# Find the item type that corresponds to the badges of each three-Elf group.
# What is the sum of the priorities of those item types?
main : Task {} *
main =
    input <- File.readUtf8 (Path.fromStr "day-3-input.txt")
        |> Task.onFail \_ -> Task.succeed "Could not read file."
        |> Task.await

    output =
        Str.trim input
        |> Str.split "\n"
        |> groupsOfThree
        |> List.map \Group a1 a2 a3 ->
            a1u8 = Str.toUtf8 a1
            a2u8 = Str.toUtf8 a2
            a3u8 = Str.toUtf8 a3

            List.findFirst a1u8 (\a -> List.contains a2u8 a && List.contains a3u8 a)
            |> Result.map (\a -> getPriority { char: a })
            |> Result.withDefault 0
        |> List.sum

    Stdout.line (Num.toStr output)

getPriority : { char : U8 } -> I64
getPriority = \{ char } ->
    if char < 97 then
        Num.toI64 char - 38
    else
        Num.toI64 char - 96

groupsOfThree : List a -> List [Group a a a]
groupsOfThree = \list ->
    when list is
        [a1, a2, a3, ..] ->
            list
            |> List.drop 3
            |> groupsOfThree
            |> List.prepend (Group a1 a2 a3)

        _ -> []
