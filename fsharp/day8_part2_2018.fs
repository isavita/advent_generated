
open System
open System.IO

let rec parseTree (data:int[]) index =
    let childCount = data.[index]
    let metaCount = data.[index+1]
    let mutable idx = index + 2
    let childValues = Array.zeroCreate childCount
    for i in 0..childCount-1 do
        let v,nidx = parseTree data idx
        childValues.[i] <- v
        idx <- nidx
    let mutable value = 0
    if childCount = 0 then
        for i in 0..metaCount-1 do value <- value + data.[idx+i]
    else
        for i in 0..metaCount-1 do
            let m = data.[idx+i]
            if m > 0 && m <= childCount then value <- value + childValues.[m-1]
    (value, idx + metaCount)

[<EntryPoint>]
let main _ =
    let data = File.ReadAllText("input.txt").Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int
    let value,_ = parseTree data 0
    printfn "%d" value
    0
