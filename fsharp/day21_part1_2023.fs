
open System
open System.IO
open System.Collections.Generic

let grid, width, height =
    let lines = File.ReadAllLines "input.txt"
    let h = lines.Length
    let w = lines.[0].Length
    let g = Array2D.init h w (fun y x -> lines.[y].[x])
    g, w, h

let startX, startY =
    let mutable sx = 0
    let mutable sy = 0
    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            if grid.[y, x] = 'S' then sx <- x; sy <- y
    sx, sy

let solve () =
    let dist = Array2D.create height width -1
    let q = Queue<(int*int)>()
    dist.[startY, startX] <- 0
    q.Enqueue (startX, startY)
    let dirs = [| (0,-1); (-1,0); (0,1); (1,0) |]
    while q.Count > 0 do
        let x,y = q.Dequeue()
        let d = dist.[y,x]
        for dx,dy in dirs do
            let nx = x + dx
            let ny = y + dy
            if nx >= 0 && nx < width && ny >= 0 && ny < height then
                if grid.[ny,nx] <> '#' && dist.[ny,nx] = -1 then
                    dist.[ny,nx] <- d + 1
                    q.Enqueue (nx,ny)
    let mutable c = 0
    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            let d = dist.[y,x]
            if d <> -1 && d <= 64 && d % 2 = 0 then c <- c + 1
    c

[<EntryPoint>]
let main _ =
    printfn "%d" (solve ())
    0
