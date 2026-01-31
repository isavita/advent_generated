
open System
open System.IO

let data = File.ReadAllText("input.txt").Split([|' '; '\n'; '\r'; '\t'|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int

let rec parse idx =
    let childCount = data.[idx]
    let metaCount = data.[idx + 1]
    let mutable i = idx + 2
    let mutable sum = 0
    for _ in 0 .. childCount - 1 do
        let (nextIdx, childSum) = parse i
        i <- nextIdx
        sum <- sum + childSum
    for _ in 0 .. metaCount - 1 do
        sum <- sum + data.[i]
        i <- i + 1
    (i, sum)

[<EntryPoint>]
let main _ =
    let (_, total) = parse 0
    printfn "%d" total
    0
