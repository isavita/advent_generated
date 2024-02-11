
module Day17

open System
open System.IO

let data = File.ReadAllText("input.txt")
let steps = int (data.Trim())
let mutable currentPos = 0
let mutable valueAfterZero = 0

for i = 1 to 50000000 do
    currentPos <- (currentPos + steps) % i
    if currentPos = 0 then
        valueAfterZero <- i
    currentPos <- currentPos + 1

printfn "%d" valueAfterZero
