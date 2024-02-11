
module Day1

let input = System.IO.File.ReadAllLines "input.txt"
let result = input |> Array.map int |> Array.sum
printfn "%d" result
