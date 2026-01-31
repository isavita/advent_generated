
open System
open System.IO
open System.Collections.Generic

let empty = '.'
let cubic = '#'
let round = 'O'

let readInput () =
    File.ReadAllLines("input.txt")
    |> Array.map (fun s -> s.ToCharArray())

let buildGrid (lines: char[][]) =
    let h = lines.Length
    let w = lines.[0].Length
    let grid = Array2D.create h w empty
    for y in 0..h-1 do
        for x in 0..w-1 do
            if lines.[y].[x] <> empty then grid.[y,x] <- lines.[y].[x]
    grid

let shift (grid: char[,]) (dx,dy) =
    let h = grid.GetLength 0
    let w = grid.GetLength 1
    let mutable moved = true
    while moved do
        moved <- false
        let ys, xs =
            if dy < 0 || dx < 0 then
                [|0..h-1|], [|0..w-1|]
            else
                [|h-1..-1..0|], [|w-1..-1..0|]
        for y in ys do
            for x in xs do
                if grid.[y,x] = round then
                    let ny, nx = y+dy, x+dx
                    if ny >= 0 && ny < h && nx >= 0 && nx < w && grid.[ny,nx] = empty then
                        grid.[ny,nx] <- round
                        grid.[y,x] <- empty
                        moved <- true

let cycle (grid: char[,]) =
    shift grid (0,-1)
    shift grid (-1,0)
    shift grid (0,1)
    shift grid (1,0)

let gridKey (grid: char[,]) =
    let mutable k = 0
    for y in 0..grid.GetLength 0 - 1 do
        for x in 0..grid.GetLength 1 - 1 do
            if grid.[y,x] = round then k <- k + x + y * grid.GetLength 1
    k

let load (grid: char[,]) =
    let h = grid.GetLength 0
    let mutable total = 0
    for y in 0..h-1 do
        for x in 0..grid.GetLength 1 - 1 do
            if grid.[y,x] = round then total <- total + (h - y)
    total

let solve () =
    let lines = readInput ()
    let grid = buildGrid lines
    let cache = Dictionary<int,int>()
    let mutable i = 0
    let target = 1000000000
    let mutable found = false
    while i < target && not found do
        let k = gridKey grid
        match cache.TryGetValue k with
        | true, start ->
            let cycleLen = i - start
            let rem = (target - start) % cycleLen
            for _ in 1..rem do cycle grid
            found <- true
        | _ ->
            cache.[k] <- i
            cycle grid
            i <- i + 1
    if not found then
        for _ in i..target-1 do cycle grid
    load grid

printfn "%d" (solve ())
