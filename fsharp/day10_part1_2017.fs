
open System
open System.IO

[<EntryPoint>]
let main argv =
    let lengths = File.ReadAllText("input.txt").Split(',') |> Array.map int
    let list = Array.init 256 id
    let mutable pos = 0
    let mutable skip = 0
    for length in lengths do
        if length > 0 then
            for k = 0 to (length / 2) - 1 do
                let start = (pos + k) % 256
                let ``end`` = (pos + length - 1 - k) % 256
                let tmp = list.[start]
                list.[start] <- list.[``end``]
                list.[``end``] <- tmp
        pos <- (pos + length + skip) % 256
        skip <- skip + 1
    printfn "%d" (list.[0] * list.[1])
    0
