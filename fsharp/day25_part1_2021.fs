
open System.IO

let moveEast (grid:char[][]) (h:int) (w:int) : bool * char[][] =
    let moved = ref false
    let oldPos = Array.init h (fun _ -> Array.create w ' ')
    for y in 0..h-1 do
        let mutable x = 0
        while x < w do
            if grid.[y].[x] = '>' then
                let nx = (x+1)%w
                if grid.[y].[nx] = '.' then
                    oldPos.[y].[x] <- '.'
                    grid.[y].[nx] <- '>'
                    moved := true
                    x <- x+1
            x <- x+1
    for y in 0..h-1 do
        for x in 0..w-1 do
            if oldPos.[y].[x] = '.' then grid.[y].[x] <- '.'
    !moved, grid

let moveSouth (grid:char[][]) (h:int) (w:int) : bool * char[][] =
    let moved = ref false
    let oldPos = Array.init h (fun _ -> Array.create w ' ')
    for x in 0..w-1 do
        let mutable y = 0
        while y < h do
            if grid.[y].[x] = 'v' then
                let ny = (y+1)%h
                if grid.[ny].[x] = '.' then
                    oldPos.[y].[x] <- '.'
                    grid.[ny].[x] <- 'v'
                    moved := true
                    y <- y+1
            y <- y+1
    for y in 0..h-1 do
        for x in 0..w-1 do
            if oldPos.[y].[x] = '.' then grid.[y].[x] <- '.'
    !moved, grid

[<EntryPoint>]
let main _ =
    let grid = File.ReadAllLines "input.txt" |> Array.map (fun s -> s.ToCharArray())
    let h = grid.Length
    let w = grid.[0].Length
    let rec step n =
        let em, g = moveEast grid h w
        let sm, _ = moveSouth g h w
        if not em && not sm then n+1 else step (n+1)
    printfn "%d" (step 0)
    0
