
module Day11

open System

let input = System.IO.File.ReadAllText("input.txt")
let serial = Int32.Parse(input.Trim())

let gridSize = 300
let grid = Array2D.create gridSize gridSize 0

for y in 0..gridSize-1 do
    for x in 0..gridSize-1 do
        let rackID = x + 11
        let powerLevel = rackID * (y + 1) + serial
        let powerLevel = ((powerLevel * rackID) / 100) % 10 - 5
        grid.[y, x] <- powerLevel

let mutable maxPower = -1 <<< 31
let mutable maxX = 0
let mutable maxY = 0

for y in 0..gridSize-3 do // Change from gridSize-2 to gridSize-3
    for x in 0..gridSize-3 do // Change from gridSize-2 to gridSize-3
        let mutable totalPower = 0
        for dy in 0..2 do
            for dx in 0..2 do
                totalPower <- totalPower + grid.[y+dy, x+dx]
        if totalPower > maxPower then
            maxPower <- totalPower
            maxX <- x+1
            maxY <- y+1

printfn "%d,%d" maxX maxY
