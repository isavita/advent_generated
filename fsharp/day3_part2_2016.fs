
open System
open System.IO

let isValid a b c = a + b > c && a + c > b && b + c > a

[<EntryPoint>]
let main _ =
    let triples =
        File.ReadAllLines "input.txt"
        |> Array.map (fun line ->
            line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int)

    let count1 =
        triples
        |> Array.sumBy (fun t -> if isValid t.[0] t.[1] t.[2] then 1 else 0)

    let mutable count2 = 0
    for i in 0 .. 3 .. triples.Length - 1 do
        if i + 2 < triples.Length then
            for j in 0 .. 2 do
                if isValid triples.[i].[j] triples.[i + 1].[j] triples.[i + 2].[j] then
                    count2 <- count2 + 1

    printfn "%d %d" count1 count2
    0
