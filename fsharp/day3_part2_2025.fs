
open System
open System.IO

let findLargest (s:string) k =
    let n = s.Length
    let mutable toRemove = n - k
    let stack = Array.zeroCreate n
    let mutable top = -1
    for i in 0 .. n - 1 do
        let c = s.[i]
        while toRemove > 0 && top >= 0 && stack.[top] < c do
            top <- top - 1; toRemove <- toRemove - 1
        top <- top + 1; stack.[top] <- c
    String(stack, 0, k)

[<EntryPoint>]
let main _ =
    File.ReadLines("input.txt")
    |> Seq.sumBy (fun line ->
        let trimmed = line.Trim()
        if trimmed.Length = 0 then 0L
        else int64 (findLargest trimmed 12))
    |> printfn "%d"
    0
