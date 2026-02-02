
open System
open System.IO

let openAcre = '.'
let trees = '|'
let lumberyard = '#'

let readInput filename =
    File.ReadAllLines(filename)
    |> Array.map (Seq.toArray)

let countAdjacent (grid: char[][]) i j acreType =
    let mutable count = 0
    for x = -1 to 1 do
        for y = -1 to 1 do
            if x <> 0 || y <> 0 then
                let ii = i + x
                let jj = j + y
                if ii >= 0 && ii < grid.Length && jj >= 0 && jj < grid.[ii].Length && grid.[ii].[jj] = acreType then
                    count <- count + 1
    count

let nextAcreState (grid: char[][]) i j =
    match grid.[i].[j] with
    | c when c = openAcre ->
        if countAdjacent grid i j trees >= 3 then trees else c
    | c when c = trees ->
        if countAdjacent grid i j lumberyard >= 3 then lumberyard else c
    | c when c = lumberyard ->
        if countAdjacent grid i j lumberyard >= 1 && countAdjacent grid i j trees >= 1 then lumberyard else openAcre
    | c -> c

let transform (grid: char[][]) =
    Array.init grid.Length (fun i ->
        Array.init grid.[i].Length (fun j -> nextAcreState grid i j))

let countResources (grid: char[][]) =
    let mutable wooded = 0
    let mutable lumberyards = 0
    for row in grid do
        for acre in row do
            if acre = trees then wooded <- wooded + 1
            elif acre = lumberyard then lumberyards <- lumberyards + 1
    (wooded, lumberyards)

let main() =
    let grid = readInput "input.txt"
    let mutable grid' = grid
    for _ = 0 to 9 do
        grid' <- transform grid'
    let (wooded, lumberyards) = countResources grid'
    printfn "%d" (wooded * lumberyards)

main()
