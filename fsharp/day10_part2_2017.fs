
open System
open System.IO

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt").Trim()
    let ascii = input |> Seq.map int |> Seq.toArray
    let lengths = Array.append ascii [|17; 31; 73; 47; 23|]

    let list = Array.zeroCreate<int> 256
    for i = 0 to 255 do list.[i] <- i

    let mutable current = 0
    let mutable skip = 0

    for _ in 0 .. 63 do
        for idx in 0 .. lengths.Length - 1 do
            let length = lengths.[idx]
            for i in 0 .. (length / 2) - 1 do
                let start = (current + i) % 256
                let endp = (current + length - 1 - i) % 256
                let tmp = list.[start]
                list.[start] <- list.[endp]
                list.[endp] <- tmp
            current <- (current + length + skip) % 256
            skip <- skip + 1

    let dense = Array.zeroCreate<byte> 16
    for i = 0 to 15 do
        let mutable x = 0
        for j = 0 to 15 do
            x <- x ^^^ list.[i * 16 + j]
        dense.[i] <- byte x

    let hex = dense |> Array.map (fun b -> b.ToString("x2")) |> String.concat ""
    printfn "%s" hex
    0
