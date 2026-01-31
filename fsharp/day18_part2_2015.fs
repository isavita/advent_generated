
open System
open System.IO

let size = 100
let steps = 100
let dirs = [| -1; 0; 1 |]

let countOn (g: bool[,]) x y =
    let mutable c = 0
    for dx in dirs do
        for dy in dirs do
            if dx <> 0 || dy <> 0 then
                let nx = x + dx
                let ny = y + dy
                if nx >= 0 && nx < size && ny >= 0 && ny < size && g.[nx, ny] then
                    c <- c + 1
    c

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let grid = Array2D.create size size false
    let newGrid = Array2D.create size size false
    for y = 0 to size-1 do
        let line = lines.[y]
        for x = 0 to size-1 do
            grid.[x, y] <- line.[x] = '#'
    grid.[0,0] <- true
    grid.[0,size-1] <- true
    grid.[size-1,0] <- true
    grid.[size-1,size-1] <- true

    for _ = 1 to steps do
        for x = 0 to size-1 do
            for y = 0 to size-1 do
                let on = countOn grid x y
                newGrid.[x, y] <-
                    if grid.[x, y] then on = 2 || on = 3
                    else on = 3
        newGrid.[0,0] <- true
        newGrid.[0,size-1] <- true
        newGrid.[size-1,0] <- true
        newGrid.[size-1,size-1] <- true
        for x = 0 to size-1 do
            for y = 0 to size-1 do
                grid.[x, y] <- newGrid.[x, y]

    let mutable onCount = 0
    for x = 0 to size-1 do
        for y = 0 to size-1 do
            if grid.[x, y] then onCount <- onCount + 1
    printfn "%d" onCount
    0
