
open System
open System.IO

[<EntryPoint>]
let main _ =
    let steps = File.ReadAllText("input.txt").Trim() |> int
    let buffer = Array.zeroCreate<int> 2018
    let mutable pos = 0

    for i = 1 to 2017 do
        pos <- (pos + steps) % i + 1
        for j = i downto pos + 1 do
            buffer.[j] <- buffer.[j - 1]
        buffer.[pos] <- i

    let answer = buffer.[(pos + 1) % 2018]
    printfn "%d" answer
    0
