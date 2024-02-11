
module Day1

let calculateFuel mass = mass / 3 - 2

let input = System.IO.File.ReadAllLines "input.txt"
let totalFuel = input |> Array.map int |> Array.sumBy calculateFuel
printfn "%d" totalFuel
