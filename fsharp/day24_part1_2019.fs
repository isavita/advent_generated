
open System
open System.Collections.Generic
open System.IO

let Side = 5
let Square = Side * Side

let parse () =
    let res = Array.zeroCreate Square
    use sr = new StreamReader("input.txt")
    let mutable row = 0
    let mutable line = sr.ReadLine()
    while not (isNull line) do
        for col in 0 .. Side - 1 do
            res.[row * Side + col] <- line.[col] = '#'
        row <- row + 1
        line <- sr.ReadLine()
    res

let biodiversity (grid: bool[]) =
    let mutable bio = 0
    for i in 0 .. Square - 1 do
        if grid.[i] then bio <- bio + (1 <<< i)
    bio

let next1 (grid: bool[]) =
    let newGrid = Array.zeroCreate Square
    for i in 0 .. Square - 1 do
        let row = i / Side
        let col = i % Side
        let mutable neighbours = 0
        if row > 0 && grid.[i - Side] then neighbours <- neighbours + 1
        if row < Side - 1 && grid.[i + Side] then neighbours <- neighbours + 1
        if col > 0 && grid.[i - 1] then neighbours <- neighbours + 1
        if col < Side - 1 && grid.[i + 1] then neighbours <- neighbours + 1
        newGrid.[i] <-
            if grid.[i] then neighbours = 1
            elif not grid.[i] then neighbours = 1 || neighbours = 2
            else grid.[i]
    newGrid

let rec findLoop (seen: HashSet<string>) grid =
    let key = String(Array.map (fun b -> if b then '#' else '.') grid)
    if seen.Contains key then
        biodiversity grid
    else
        seen.Add key |> ignore
        findLoop seen (next1 grid)

[<EntryPoint>]
let main _ =
    let grid = parse ()
    printfn "%d" (findLoop (HashSet<string>()) grid)
    0
