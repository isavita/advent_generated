
module Day15

open System.IO

let input = File.ReadAllText "input.txt"

let hashAlgorithm (s: string) =
    let mutable current = 0
    for c in s do
        let ascii = int c
        current <- (current + ascii) * 17 % 256
    current

let result = input.Split([|','|])
                |> Array.map (fun step -> hashAlgorithm step)
                |> Array.sum

printfn "%d" result
