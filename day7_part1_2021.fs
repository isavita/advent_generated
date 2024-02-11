
module Day7

open System
open System.IO
open System.Collections.Generic

let file = File.ReadAllLines "input.txt"
let positions = file |> Array.collect (fun line -> line.Split(',')) |> Array.map int |> Array.sort

let mutable min_fuel = Int32.MaxValue

let calculateFuel currentPosition newPosition = abs (currentPosition - newPosition)
let abs n = if n < 0 then -n else n

for i in positions.[0] .. positions.[positions.Length - 1] do
    let mutable fuel = 0
    for pos in positions do
        fuel <- fuel + calculateFuel pos i
    if fuel < min_fuel then
        min_fuel <- fuel

printfn "%d" min_fuel
