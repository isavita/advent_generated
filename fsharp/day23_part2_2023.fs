open System
open System.Collections.Generic
open System.Linq

type Coord = { X: int; Y: int }
type Edge = { Start: Coord; End: Coord; Weight: int }

let north = { X = 0; Y = -1 }
let south = { X = 0; Y = 1 }
let west  = { X = -1; Y = 0 }
let east  = { X = 1; Y = 0 }
let dir4 = [| north; south; west; east |]

let inBounds (c: Coord) w h = c.X >= 0 && c.X < w && c.Y >= 0 && c.Y < h
let add (a: Coord) (b: Coord) = { X = a.X + b.X; Y = a.Y + b.Y }

let parse (lines: string[]) =
    let h = lines.Length
    let w = if h = 0 then 0 else lines.[0].Length
    let dict = Dictionary<Coord, char>()
    for y in 0 .. h - 1 do
        for x in 0 .. w - 1 do
            let ch = lines.[y].[x]
            if ch <> '.' then dict.[{ X = x; Y = y }] <- ch
    dict, w, h

let neighbors (c: Coord) (data: Dictionary<Coord, char>) w h =
    [ for d in dir4 do
        let n = add c d
        if inBounds n w h && (not (data.ContainsKey n) || data.[n] <> '#') then yield n ]

let bfsEdges (start: Coord) (data: Dictionary<Coord, char>) w h (verts: HashSet<Coord>) =
    let q = Queue<Coord>()
    let reached = HashSet<Coord>()
    let dist = Dictionary<Coord, int>()
    q.Enqueue(start)
    reached.Add(start) |> ignore
    dist.[start] <- 0
    let edges = HashSet<Edge>()
    while q.Count > 0 do
        let cur = q.Dequeue()
        if verts.Contains(cur) && cur <> start then
            edges.Add({ Start = start; End = cur; Weight = dist.[cur] }) |> ignore
        else
            for n in neighbors cur data w h do
                if reached.Add(n) then
                    dist.[n] <- dist.[cur] + 1
                    q.Enqueue(n)
    edges

let buildGraph (data: Dictionary<Coord, char>) w h (start: Coord) (endC: Coord) =
    let verts = HashSet<Coord>()
    verts.Add(start) |> ignore
    verts.Add(endC) |> ignore
    for y in 0 .. h - 1 do
        for x in 0 .. w - 1 do
            let c = { X = x; Y = y }
            let isWall = data.ContainsKey c && data.[c] = '#'
            if not isWall then
                let nb = neighbors c data w h
                if List.length nb > 2 then verts.Add c |> ignore
    let edges = Dictionary<Coord, List<Edge>>()
    for v in verts do
        edges.[v] <- (bfsEdges v data w h verts).ToList()
    verts, edges

let rec dfs (cur: Coord) (endC: Coord) (edges: Dictionary<Coord, List<Edge>>) (seen: HashSet<Coord>) =
    if cur = endC then 0
    else
        seen.Add(cur) |> ignore
        let mutable best = -1
        match edges.TryGetValue(cur) with
        | true, list ->
            for e in list do
                if not (seen.Contains e.End) then
                    let d = dfs e.End endC edges seen
                    if d >= 0 then best <- Math.Max(best, d + e.Weight)
        | _ -> ()
        seen.Remove(cur) |> ignore
        best

let solve (lines: string[]) =
    let data, w, h = parse lines
    let start = { X = 1; Y = 0 }
    let endC = { X = w - 2; Y = h - 1 }
    let _, edges = buildGraph data w h start endC
    let seen = HashSet<Coord>()
    dfs start endC edges seen

[<EntryPoint>]
let main argv =
    let lines = System.IO.File.ReadAllLines("input.txt")
    let ans = solve lines
    printfn "%d" ans
    0