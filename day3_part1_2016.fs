
module Day3

open System.IO

let isValidTriangle (a:int) (b:int) (c:int) = a + b > c && a + c > b && b + c > a

let countValidTriangles (input:string) =
    let lines = File.ReadAllLines input
    let mutable count = 0
    for line in lines do
        let sides = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int
        if isValidTriangle sides.[0] sides.[1] sides.[2] then count <- count + 1
    count

let result = countValidTriangles "input.txt"
printfn "%d" result
