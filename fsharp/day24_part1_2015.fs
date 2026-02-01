
open System
open System.IO

[<EntryPoint>]
let main argv =
    let packages = File.ReadAllLines("input.txt") |> Array.map int
    let targetWeight = (Array.sum packages) / 3
    let mutable bestLength = Int32.MaxValue
    let mutable bestQE = Int64.MaxValue

    let rec dfs idx curQE curLen curWeight =
        if curWeight = targetWeight then
            if curLen < bestLength || (curLen = bestLength && curQE < bestQE) then
                bestLength <- curLen
                bestQE <- curQE
        elif idx = packages.Length || curLen >= bestLength || curWeight > targetWeight then
            ()
        else
            let w = packages.[idx]
            dfs (idx + 1) (curQE * int64 w) (curLen + 1) (curWeight + w)
            dfs (idx + 1) curQE curLen curWeight

    dfs 0 1L 0 0
    printfn "%d" bestQE
    0
