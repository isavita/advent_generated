open System
open System.IO

let readInput (file:string) =
    File.ReadAllLines file
    |> Array.map (fun line -> line.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let dirs = [| (-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1) |]

let rec flash (grid:int[][]) x y (flashed:bool[,]) =
    if flashed.[y,x] then 0
    else
        flashed.[y,x] <- true
        let mutable cnt = 1
        for dx,dy in dirs do
            let nx = x+dx
            let ny = y+dy
            if nx>=0 && nx<grid.[0].Length && ny>=0 && ny<grid.Length then
                grid.[ny].[nx] <- grid.[ny].[nx] + 1
                if grid.[ny].[nx] > 9 then cnt <- cnt + flash grid nx ny flashed
        cnt

let simulate (grid:int[][]) =
    let rows = grid.Length
    let cols = grid.[0].Length
    let flashed = Array2D.create rows cols false
    for y=0 to rows-1 do
        for x=0 to cols-1 do
            grid.[y].[x] <- grid.[y].[x] + 1
    let mutable total = 0
    for y=0 to rows-1 do
        for x=0 to cols-1 do
            if grid.[y].[x] > 9 then total <- total + flash grid x y flashed
    for y=0 to rows-1 do
        for x=0 to cols-1 do
            if flashed.[y,x] then grid.[y].[x] <- 0
    total

[<EntryPoint>]
let main _ =
    let grid = readInput "input.txt"
    let rows = grid.Length
    let cols = grid.[0].Length
    let mutable step = 0
    while true do
        step <- step + 1
        let f = simulate grid
        if f = rows * cols then
            printfn "%d" step
            Environment.Exit 0
    0