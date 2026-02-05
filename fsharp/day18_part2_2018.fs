open System
open System.IO
open System.Collections.Generic
open System.Text

let openChar = '.'
let treesChar = '|'
let lumberChar = '#'

let readInput (filename:string) =
    File.ReadAllLines(filename) |> Array.map (fun l -> l.ToCharArray())

let countAdjacent (grid:char[][]) i j target =
    let mutable cnt = 0
    for dx = -1 to 1 do
        for dy = -1 to 1 do
            if not (dx = 0 && dy = 0) then
                let ni = i + dx
                let nj = j + dy
                if ni >= 0 && ni < grid.Length && nj >= 0 && nj < grid.[0].Length && grid.[ni].[nj] = target then
                    cnt <- cnt + 1
    cnt

let nextState (grid:char[][]) i j =
    match grid.[i].[j] with
    | '.' -> if countAdjacent grid i j treesChar >= 3 then treesChar else openChar
    | '|' -> if countAdjacent grid i j lumberChar >= 3 then lumberChar else treesChar
    | '#' ->
        if countAdjacent grid i j lumberChar >= 1 && countAdjacent grid i j treesChar >= 1 then lumberChar
        else openChar
    | c -> c

let transform (grid:char[][]) =
    let rows = grid.Length
    let cols = grid.[0].Length
    Array.init rows (fun i -> Array.init cols (fun j -> nextState grid i j))

let countResources (grid:char[][]) =
    let mutable wooded = 0
    let mutable lumber = 0
    for row in grid do
        for c in row do
            if c = treesChar then wooded <- wooded + 1
            elif c = lumberChar then lumber <- lumber + 1
    (wooded, lumber)

let gridToString (grid:char[][]) =
    let sb = StringBuilder()
    for row in grid do
        sb.AppendLine(String(row)) |> ignore
    sb.ToString()

[<EntryPoint>]
let main argv =
    let mutable grid = readInput "input.txt"
    let seen = Dictionary<string,int>()
    let mutable cycleStart = 0
    let mutable cycleLen = 0
    let mutable minute = 0
    let mutable found = false
    while not found do
        let state = gridToString grid
        if seen.ContainsKey(state) then
            cycleStart <- seen.[state]
            cycleLen <- minute - cycleStart
            found <- true
        else
            seen.[state] <- minute
            grid <- transform grid
            minute <- minute + 1
    let remaining = (1000000000 - cycleStart) % cycleLen
    for _ = 0 to remaining - 1 do
        grid <- transform grid
    let (wooded, lumber) = countResources grid
    printfn "%d" (wooded * lumber)
    0