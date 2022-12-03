app "day-3-part-2"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.1/zAoiC9xtQPHywYk350_b7ust04BmWLW00sjb9ZPtSQk.tar.br" }
    imports [
        pf.Stdout,
        pf.File,
        pf.Task.{ await, Task },
        pf.Path.{ Path },
    ]
    provides [main] to pf

# --- Day 3: Rucksack Reorganization ---
# One Elf has the important job of loading all of the rucksacks with supplies for
# the jungle journey. Unfortunately, that Elf didn't quite follow the packing
# instructions, and so a few items now need to be rearranged.
#
# Each rucksack has two large compartments. All items of a given type are meant
# to go into exactly one of the two compartments. The Elf that did the packing
# failed to follow this rule for exactly one item type per rucksack.
#
# The Elves have made a list of all of the items currently in each rucksack
# (your puzzle input), but they need your help finding the errors. Every item
# type is identified by a single lowercase or uppercase letter (that is, a and A
# refer to different types of items).
#
# The list of items for each rucksack is given as characters all on a single line.
# A given rucksack always has the same number of items in each of its two compartments,
#  so the first half of the characters represent items in the first compartment,
# while the second half of the characters represent items in the second compartment.
#
# For example, suppose you have the following list of contents from six rucksacks:
#
#    vJrwpWtwJgWrhcsFMMfFFhFp
#    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
#    PmmdzqPrVvPwwTWBwg
#    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
#    ttgJtRGJQctTZtZT
#    CrZsJsPPZsGzwwsLwLmpwMDw
#
# - The first rucksack contains the items vJrwpWtwJgWrhcsFMMfFFhFp, which means
#   its first compartment contains the items vJrwpWtwJgWr, while the second compartment
#   contains the items hcsFMMfFFhFp. The only item type that appears in both
#   compartments is lowercase p.
# - The second rucksack's compartments contain jqHRNqRjqzjGDLGL and rsFMfFZSrLrFZsSL. T
#   he only item type that appears in both compartments is uppercase L.
# - The third rucksack's compartments contain PmmdzqPrV and vPwwTWBwg; the only
#   common item type is uppercase P.
# - The fourth rucksack's compartments only share item type v.
# - The fifth rucksack's compartments only share item type t.
# - The sixth rucksack's compartments only share item type s.
#
# To help prioritize item rearrangement, every item type can be converted to a priority:
#
# - Lowercase item types a through z have priorities 1 through 26.
# - Uppercase item types A through Z have priorities 27 through 52.
#
# In the above example, the priority of the item type that appears in both
# compartments of each rucksack is 16 (p), 38 (L), 42 (P), 22 (v), 20 (t), and 19 (s);
# the sum of these is 157.
#
# Find the item type that appears in both compartments of each rucksack. What is
# the sum of the priorities of those item types?
main : Task {} *
main =
    input <- File.readUtf8 (Path.fromStr "day-3-input.txt")
        |> Task.onFail \_ -> Task.succeed "Could not read file."
        |> Task.await

    output =
        Str.trim input
        |> Str.split "\n"
        |> List.map \str ->
            { before, others } = List.split (Str.toUtf8 str) (Str.countGraphemes str // 2)

            # dbg T before others
            List.findFirst before (\a -> List.contains others a)
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
