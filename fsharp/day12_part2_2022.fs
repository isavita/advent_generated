
open System
open System.IO
open System.Collections.Generic

[<Struct>]
type Point = { X:int; Y:int }

let neighbors = [| (0,1); (0,-1); (1,0); (-1,0) |]

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let grid = Dictionary<Point, byte>()
    let mutable start = Unchecked.defaultof<Point>
    let mutable finish = Unchecked.defaultof<Point>
    let asList = ResizeArray<Point>()

    for y = 0 to lines.Length-1 do
        let line = lines.[y]
        for x = 0 to line.Length-1 do
            let p = { X = x; Y = y }
            let b = byte line.[x]
            grid.[p] <- b
            if b = byte 'S' then start <- p
            elif b = byte 'E' then finish <- p
            elif b = byte 'a' then asList.Add p

    grid.[start] <- byte 'a'
    grid.[finish] <- byte 'z'

    let dijkstra (src:Point) =
        let pq = PriorityQueue<Point,int>()
        let dist = Dictionary<Point,int>()
        dist.[src] <- 0
        pq.Enqueue(src,0)
        while pq.Count > 0 do
            let cur = pq.Dequeue()
            let curDist = dist.[cur]
            for (dx,dy) in neighbors do
                let nxt = { X = cur.X + dx; Y = cur.Y + dy }
                if grid.ContainsKey nxt && int grid.[cur] - int grid.[nxt] <= 1 then
                    let nd = curDist + 1
                    match dist.TryGetValue nxt with
                    | true, old when nd < old -> dist.[nxt] <- nd; pq.Enqueue(nxt, nd)
                    | false, _                -> dist.[nxt] <- nd; pq.Enqueue(nxt, nd)
                    | _                       -> ()
        dist

    let dists = dijkstra finish
    let mutable best = Int32.MaxValue
    match dists.TryGetValue start with
    | true, v -> best <- v
    | _ -> ()
    for a in asList do
        match dists.TryGetValue a with
        | true, v when v < best -> best <- v
        | _ -> ()

    printfn "%d" best
    0
