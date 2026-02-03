
open System
open System.IO

let readInput () =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let dirs = [| (-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1) |]

let rec flash (grid:int[][]) x y (flashed:bool[,]) =
    if flashed.[y,x] then 0 else
    flashed.[y,x] <- true
    let mutable c = 1
    for dx,dy in dirs do
        let nx = x+dx
        let ny = y+dy
        if nx>=0 && nx<grid.[0].Length && ny>=0 && ny<grid.Length then
            grid.[ny].[nx] <- grid.[ny].[nx] + 1
            if grid.[ny].[nx] > 9 then c <- c + flash grid nx ny flashed
    c

let step (grid:int[][]) =
    let rows = grid.Length
    let cols = grid.[0].Length
    let flashed = Array2D.create rows cols false
    for y in 0 .. rows-1 do
        for x in 0 .. cols-1 do
            grid.[y].[x] <- grid.[y].[x] + 1
    let mutable f = 0
    for y in 0 .. rows-1 do
        for x in 0 .. cols-1 do
            if grid.[y].[x] > 9 then f <- f + flash grid x y flashed
    for y in 0 .. rows-1 do
        for x in 0 .. cols-1 do
            if flashed.[y,x] then grid.[y].[x] <- 0
    f

[<EntryPoint>]
let main argv =
    let grid = readInput()
    let mutable total = 0
    for _ in 0 .. 99 do total <- total + step grid
    printfn "%d" total
    0
