
open System
open System.IO

let n = File.ReadAllText("input.txt").Trim() |> int

let mutable p = 1
while p * 3 <= n do
    p <- p * 3

let ans =
    if n = p then n
    elif n <= 2 * p then n - p
    else 2 * n - 3 * p

[<EntryPoint>]
let main _ =
    printfn "%d" ans
    0
