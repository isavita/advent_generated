module Day6Part2

open System
open System.IO

// Define the grid size
let gridSize = 1000
let grid = Array2D.create gridSize gridSize 0

// Function to apply an instruction to the grid
let applyInstruction (instruction: string) =
    let parts = instruction.Split(' ')
    let command =
        if instruction.StartsWith("toggle") then "toggle"
        elif instruction.Contains("turn on") then "turn on"
        else "turn off"
    let coords = parts.[parts.Length - 3].Split(',') |> Array.map int
    let endCoords = parts.[parts.Length - 1].Split(',') |> Array.map int
    let startX = coords.[0]
    let startY = coords.[1]
    let endX = endCoords.[0]
    let endY = endCoords.[1]

    for x in startX .. endX do
        for y in startY .. endY do
            match command with
            | "toggle" -> grid.[x, y] <- grid.[x, y] + 2
            | "turn on" -> grid.[x, y] <- grid.[x, y] + 1
            | "turn off" -> grid.[x, y] <- max (grid.[x, y] - 1) 0
            | _ -> ()

// Read instructions from file and apply them
let processInstructions (filename: string) =
    File.ReadAllLines(filename)
    |> Array.iter applyInstruction

// Manually calculate the total brightness
let calculateTotalBrightness () =
    let mutable totalBrightness = 0
    for x in 0 .. gridSize - 1 do
        for y in 0 .. gridSize - 1 do
            totalBrightness <- totalBrightness + grid.[x, y]
    totalBrightness

let filename = "input.txt"
processInstructions filename
let totalBrightness = calculateTotalBrightness ()
printfn "%d" totalBrightness

