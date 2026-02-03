
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines("input.txt")
    let height = lines.Length
    let width = lines.[0].Length
    let map = lines |> Array.map (fun s -> s.ToCharArray())
    let mutable sx = 0
    let mutable sy = 0
    let mutable ex = 0
    let mutable ey = 0
    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            match map.[y].[x] with
            | 'S' -> sx <- x; sy <- y
            | 'E' -> ex <- x; ey <- y
            | _ -> ()
    let visited = Array2D.create height width false
    let q = Queue<(int * int * int * char)>()
    q.Enqueue (sx, sy, 0, 'a')
    visited.[sy, sx] <- true
    let dx = [| 0; 0; 1; -1 |]
    let dy = [| 1; -1; 0; 0 |]
    let mutable result = -1
    while q.Count > 0 && result = -1 do
        let (x, y, d, e) = q.Dequeue()
        if x = ex && y = ey then result <- d
        else
            for i in 0 .. 3 do
                let nx = x + dx.[i]
                let ny = y + dy.[i]
                if nx >= 0 && nx < width && ny >= 0 && ny < height && not visited.[ny, nx] then
                    let ne = map.[ny].[nx]
                    let from = int e - int 'a'
                    let to_ = if ne = 'E' then 25 else int ne - int 'a'
                    if to_ - from <= 1 then
                        visited.[ny, nx] <- true
                        q.Enqueue (nx, ny, d + 1, ne)
    printfn "%d" result
    0
