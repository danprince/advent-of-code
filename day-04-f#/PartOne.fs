// http://adventofcode.com/2017/day/4

open System

let alreadyHasWord (used, valid) word =
  if Set.contains word used then
    (used, false)
  else
    (Set.add word used, valid)

let isValidPassphrase (str: string) =
  let words = str.Split ' ' |> Array.toList
  List.fold alreadyHasWord (Set.empty, true) words |> snd

let solve passphrases =
  List.filter isValidPassphrase passphrases |> List.length
