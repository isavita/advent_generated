
open System
open System.IO
open System.Collections.Generic

type Point = { x: int; y: int }

let directions = [| (1, 0); (-1, 0); (0, 1); (0, -1) |]

let readInput () =
    let lines = File.ReadAllLines "input.txt"
    let h = lines.Length
    let w = lines |> Array.map String.length |> Array.max
    let grid = Array.init h (fun i ->
        let row = lines.[i]
        let arr = Array.create w ' '
        for j in 0 .. row.Length - 1 do
            arr.[j] <- row.[j]
        arr)
    let walls = Array.init h (fun i ->
        Array.init w (fun j -> grid.[i].[j] = '#'))
    let mutable s = { x = 0; y = 0 }
    let mutable e = { x = 0; y = 0 }
    let track = ResizeArray<Point>()
    for i in 0 .. h - 1 do
        for j in 0 .. w - 1 do
            let ch = grid.[i].[j]
            if ch = 'S' then s <- { x = i; y = j }
            elif ch = 'E' then e <- { x = i; y = j }
            if ch <> ' ' && ch <> '#' then track.Add { x = i; y = j }
    h, w, walls, s, e, track

let bfs (h:int) (w:int) (walls:bool[][]) (start:Point) =
    let dist = Array.init h (fun _ -> Array.create w -1)
    let queue = Array.create (h * w) (0, 0)
    let mutable head = 0
    let mutable tail = 0
    dist.[start.x].[start.y] <- 0
    queue.[tail] <- (start.x, start.y)
    tail <- tail + 1
    while head < tail do
        let (x, y) = queue.[head]
        head <- head + 1
        let d = dist.[x].[y]
        for (dx, dy) in directions do
            let nx = x + dx
            let ny = y + dy
            if nx >= 0 && nx < h && ny >= 0 && ny < w && not walls.[nx].[ny] && dist.[nx].[ny] = -1 then
                dist.[nx].[ny] <- d + 1
                queue.[tail] <- (nx, ny)
                tail <- tail + 1
    dist

[<EntryPoint>]
let main _ =
    let h, w, walls, s, e, track = readInput()
    let distS = bfs h w walls s
    let distE = bfs h w walls e
    let normalCost = distS.[e.x].[e.y]
    if normalCost = -1 then
        printfn "0"
    else
        let mutable count = 0
        for startPos in track do
            let sd = distS.[startPos.x].[startPos.y]
            if sd <> -1 then
                for (dx1, dy1) in directions do
                    let m1x = startPos.x + dx1
                    let m1y = startPos.y + dy1
                    if m1x >= 0 && m1x < h && m1y >= 0 && m1y < w then
                        for (dx2, dy2) in directions do
                            let m2x = m1x + dx2
                            let m2y = m1y + dy2
                            if m2x >= 0 && m2x < h && m2y >= 0 && m2y < w && not walls.[m2x].[m2y] then
                                let ed = distE.[m2x].[m2y]
                                if ed <> -1 then
                                    let newCost = sd + 2 + ed
                                    if normalCost - newCost >= 100 then
                                        count <- count + 1
        printfn "%d" count
    0
