open System
open System.IO
open System.Collections.Generic

type Point = { X:int; Y:int }
type Blizzard = { X:int; Y:int; Dir:char }
type State = { X:int; Y:int; T:int }

let rec gcd (a:int64) (b:int64) =
    if b = 0L then abs a else gcd b (a % b)
let lcm (a:int64) (b:int64) =
    if a = 0L || b = 0L then 0L else System.Math.Abs((a / (gcd a b)) * b)

let readInput path =
    let walls = HashSet<Point>()
    let blizzards = List<Blizzard>()
    let lines = File.ReadAllLines path
    let h = lines.Length
    let w = lines.[0].Length
    for y in 0 .. h - 1 do
        let line = lines.[y]
        for x in 0 .. w - 1 do
            let c = line.[x]
            if c = '#' then walls.Add({ X = x; Y = y }) |> ignore
            elif c = '<' || c = '>' || c = '^' || c = 'v' then
                blizzards.Add({ X = x; Y = y; Dir = c })
    walls, blizzards, h, w

let findStartEnd (walls: HashSet<Point>) (h:int) (w:int) =
    let mutable sX = -1
    for x in 0 .. w - 1 do
        if sX = -1 && not (walls.Contains({ X = x; Y = 0 })) then sX <- x
    let start = { X = sX; Y = 0 }

    let mutable eX = -1
    for x in 0 .. w - 1 do
        if eX = -1 && not (walls.Contains({ X = x; Y = h - 1 })) then eX <- x
    let endP = { X = eX; Y = h - 1 }

    start, endP

let precompute (blizzards: List<Blizzard>) (w:int) (h:int) (period:int) =
    let grid = Array3D.zeroCreate<bool> period h w
    let iw = w - 2
    let ih = h - 2
    for t in 0 .. period - 1 do
        for b in blizzards do
            let nx, ny =
                match b.Dir with
                | '>' -> (1 + ((b.X - 1 + t) % iw), b.Y)
                | '<' -> (1 + (((b.X - 1 - t) % iw + iw) % iw), b.Y)
                | 'v' -> (b.X, 1 + ((b.Y - 1 + t) % ih))
                | '^' -> (b.X, 1 + (((b.Y - 1 - t) % ih + ih) % ih))
                | _ -> (b.X, b.Y)
            grid.[t, ny, nx] <- true
    grid

let bfs (startP: Point) (endP: Point) (walls: HashSet<Point>) (blizz: bool[,,]) (period:int) (w:int) (h:int) (startTime:int) =
    let q = Queue<int * int * int>()
    let visited = HashSet<State>()
    q.Enqueue(startP.X, startP.Y, startTime)
    visited.Add({ X = startP.X; Y = startP.Y; T = startTime % period })
    let deltas : (int * int)[] = [| (0,0); (1,0); (-1,0); (0,1); (0,-1) |]
    let mutable ans = -1
    while q.Count > 0 && ans = -1 do
        let x, y, t = q.Dequeue()
        if x = endP.X && y = endP.Y then
            ans <- t
        else
            let nt = t + 1
            let ntp = nt % period
            for dx, dy in deltas do
                let nx = x + dx
                let ny = y + dy
                if nx = endP.X && ny = endP.Y && not blizz.[ntp, ny, nx] then
                    ans <- nt
                else
                    if nx = startP.X && ny = startP.Y && not blizz.[ntp, ny, nx] then
                        let s = { X = nx; Y = ny; T = ntp }
                        if visited.Add(s) then q.Enqueue(nx, ny, nt) |> ignore
                    elif nx >= 1 && nx < w - 1 && ny >= 1 && ny < h - 1 then
                        if not (walls.Contains({ X = nx; Y = ny })) && not blizz.[ntp, ny, nx] then
                            let s = { X = nx; Y = ny; T = ntp }
                            if visited.Add(s) then q.Enqueue(nx, ny, nt)
    ans

[<EntryPoint>]
let main argv =
    let (walls, blizzards, h, w) = readInput "input.txt"
    let startP, endP = findStartEnd walls h w
    let w2 = w - 2
    let h2 = h - 2
    let period64 = lcm (int64 w2) (int64 h2)
    let period = int period64
    let grid = precompute blizzards w h period
    let t1 = bfs startP endP walls grid period w h 0
    let t2 = bfs endP startP walls grid period w h t1
    let t3 = bfs startP endP walls grid period w h t2
    printfn "%d" t3
    0