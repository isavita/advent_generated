
open System
open System.IO
open System.Collections.Generic

type Point = { X:int; Y:int }

let move p c =
    match c with
    | 'N' -> { p with Y = p.Y - 1 }
    | 'S' -> { p with Y = p.Y + 1 }
    | 'E' -> { p with X = p.X + 1 }
    | 'W' -> { p with X = p.X - 1 }
    | _   -> p

let buildMap (regex:string) =
    let dm = Dictionary<Point, HashSet<Point>>()
    let stack = Stack<Point>()
    let mutable cp = { X = 0; Y = 0 }
    for c in regex do
        match c with
        | '(' -> stack.Push(cp)
        | '|' -> cp <- stack.Peek()
        | ')' -> cp <- stack.Pop()
        | _ ->
            let np = move cp c
            let hs = 
                if dm.ContainsKey(cp) then dm.[cp]
                else let hs = HashSet<Point>() in dm.[cp] <- hs; hs
            hs.Add(np) |> ignore
            cp <- np
    dm

let findFurthestRoom (dm:Dictionary<Point, HashSet<Point>>) =
    let visited = Dictionary<Point, int>()
    let q = Queue<Point>()
    let start = { X = 0; Y = 0 }
    q.Enqueue(start)
    visited.[start] <- 0
    let mutable maxDist = 0
    while q.Count > 0 do
        let p = q.Dequeue()
        match dm.TryGetValue(p) with
        | true, neigh ->
            for np in neigh do
                if not (visited.ContainsKey(np)) then
                    let d = visited.[p] + 1
                    visited.[np] <- d
                    if d > maxDist then maxDist <- d
                    q.Enqueue(np)
        | _ -> ()
    maxDist

[<EntryPoint>]
let main _ =
    let data = File.ReadAllText("input.txt").Trim()
    if data.Length < 2 then
        printfn "0"
    else
        let regex = data.Substring(1, data.Length - 2)
        let dm = buildMap regex
        let ans = findFurthestRoom dm
        printfn "%d" ans
    0
