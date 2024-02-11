
module Day2

open System.IO

let input = File.ReadAllLines "input.txt"

let countChars (str: string) =
    str
    |> Seq.groupBy id
    |> Seq.map (fun (c, group) -> c, Seq.length group)
    |> Map.ofSeq

let hasExactlyNChars (n: int) (counts: Map<char, int>) =
    counts
    |> Map.exists (fun _ count -> count = n)

let counts = input |> Array.map countChars

let twos = counts |> Array.filter (hasExactlyNChars 2)
let threes = counts |> Array.filter (hasExactlyNChars 3)

let checksum = twos.Length * threes.Length

printfn "%d" checksum
