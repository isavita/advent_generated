
open System
open System.IO

[<EntryPoint>]
let main _ =
    let target = 14360655L
    let numbers = File.ReadAllLines("input.txt") |> Array.map Int64.Parse
    let mutable start = 0
    let mutable sum = 0L
    let mutable result = 0L
    let n = numbers.Length
    while start < n && result = 0L do
        let mutable endIdx = start
        sum <- 0L
        while endIdx < n && sum < target do
            sum <- sum + numbers.[endIdx]
            endIdx <- endIdx + 1
        if sum = target then
            let slice = numbers.[start..endIdx-1]
            result <- (Array.min slice) + (Array.max slice)
        else
            start <- start + 1
    printfn "%d" result
    0
