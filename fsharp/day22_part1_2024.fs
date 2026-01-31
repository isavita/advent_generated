
open System
open System.IO

let inline nextSecret (s: uint64) =
    let mutable s = s
    s <- (s ^^^ (s * 64UL)) &&& 0xFFFFFFUL
    s <- (s ^^^ (s >>> 5))   &&& 0xFFFFFFUL
    (s ^^^ (s * 2048UL)) &&& 0xFFFFFFUL

let secret2000 s0 =
    let mutable s = s0
    for _ in 1..2000 do
        s <- nextSecret s
    s

[<EntryPoint>]
let main _ =
    let total =
        File.ReadAllLines "input.txt"
        |> Array.sumBy (fun l -> secret2000 (uint64 l))
    printfn "%d" total
    0
