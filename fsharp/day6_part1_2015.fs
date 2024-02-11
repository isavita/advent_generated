module Day6Part1

open System
open System.IO

// Define the grid size
let gridSize = 1000
let grid = Array2D.create gridSize gridSize false

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
            | "toggle" -> grid.[x, y] <- not grid.[x, y]
            | "turn on" -> grid.[x, y] <- true
            | "turn off" -> grid.[x, y] <- false
            | _ -> ()

// Read instructions from file and apply them
let processInstructions (filename: string) =
    File.ReadAllLines(filename)
    |> Array.iter applyInstruction

// Manually count the lights that are on
let countLightsOn () =
    let mutable count = 0
    for x in 0 .. gridSize - 1 do
        for y in 0 .. gridSize - 1 do
            if grid.[x, y] then
                count <- count + 1
    count

// Adjusted section for execution without an EntryPoint
let filename = "input.txt" // Specify the correct path to your input file
processInstructions filename
let lightsOn = countLightsOn ()
printfn "%d" lightsOn

