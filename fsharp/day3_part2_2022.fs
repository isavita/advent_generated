
open System
open System.IO

let priority c =
    if c >= 'a' && c <= 'z' then int (c - 'a') + 1
    else int (c - 'A') + 27

let toSet (s: string) =
    s.ToCharArray() |> Set.ofArray

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let mutable sum = 0
    for i in 0..3..lines.Length-1 do
        let a = toSet lines.[i]
        let b = toSet lines.[i+1]
        let c = toSet lines.[i+2]
        let common = Set.intersect a (Set.intersect b c) |> Seq.head
        sum <- sum + priority common
    printfn "%d" sum
    0
