
open System
open System.IO
open System.Collections.Generic

type Point = struct
    val x: int64
    val y: int64
    new(x, y) = { x = x; y = y }
end

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let size = lines.Length
    let grid = Array2D.create size size '.'
    let mutable sx = -1
    let mutable sy = -1
    for y = 0 to size - 1 do
        let line = lines.[y]
        for x = 0 to size - 1 do
            let c = line.[x]
            if c = 'S' then
                sx <- x
                sy <- y
                grid.[y, x] <- '.'
            else
                grid.[y, x] <- c
    let dx = [| 0; 0; 1; -1 |]
    let dy = [| 1; -1; 0; 0 |]

    let doneArr = Array.zeroCreate<int64> 3
    let mutable doneCnt = 0
    let mutable cur = HashSet<Point>()
    cur.Add(Point(int64 sx, int64 sy)) |> ignore

    let targetMod = (size - 1) / 2
    let mutable step = 0
    while doneCnt < 3 do
        if step % size = targetMod then
            doneArr.[doneCnt] <- int64 cur.Count
            doneCnt <- doneCnt + 1
        let next = HashSet<Point>()
        for p in cur do
            for k = 0 to 3 do
                let nx = p.x + int64 dx.[k]
                let ny = p.y + int64 dy.[k]
                let mx = ((nx % int64 size) + int64 size) % int64 size
                let my = ((ny % int64 size) + int64 size) % int64 size
                if grid.[int my, int mx] <> '#' then
                    next.Add(Point(nx, ny)) |> ignore
        cur <- next
        step <- step + 1

    let n = 26501365L / int64 size
    let a = doneArr.[0]
    let b = doneArr.[1]
    let c = doneArr.[2]
    let res =
        a
        + n * (b - a)
        + (n * (n - 1L) / 2L) * (c - 2L * b + a)

    printfn "%d" res
    0
