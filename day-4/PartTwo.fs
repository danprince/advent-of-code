// http://adventofcode.com/2017/day/4

open System

let sortString (str: string) = str |> Seq.sort |> String.Concat

let alreadyHasWord (used, valid) word =
  let sorted = sortString word

  if Set.contains sorted used then
    (used, false)
  else
    (Set.add word sorted, valid)

let isValidPassphrase (str: string) =
    let words = str.Split ' ' |> Array.toList
    List.fold alreadyHasWord (Set.empty, true) words |> snd

let solve passphrases =
  List.filter isValidPassphrase passphrases |> List.length
