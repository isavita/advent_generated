
open System
open System.Collections.Generic
open System.IO

type Point = { x: int; y: int }

let equals (p1: Point) (p2: Point) =
    p1.x = p2.x && p1.y = p2.y

let hash (p: Point) =
    p.x.GetHashCode() ^^^ p.y.GetHashCode()

let move (p: Point) dir =
    match dir with
    | 'N' -> { x = p.x; y = p.y - 1 }
    | 'S' -> { x = p.x; y = p.y + 1 }
    | 'E' -> { x = p.x + 1; y = p.y }
    | 'W' -> { x = p.x - 1; y = p.y }
    | _ -> p

let buildMap (regex: string) =
    let dm = new Dictionary<Point, HashSet<Point>>()
    let stack = new Stack<Point>()
    let mutable cp = { x = 0; y = 0 }

    for c in regex.Substring(1, regex.Length - 2) do
        match c with
        | '(' -> stack.Push(cp)
        | '|' -> cp <- stack.Peek()
        | ')' -> cp <- stack.Pop()
        | dir ->
            let np = move cp dir
            if not (dm.ContainsKey cp) then
                dm.[cp] <- HashSet()
            dm.[cp].Add(np) |> ignore
            if not (dm.ContainsKey np) then
                dm.[np] <- HashSet()
            dm.[np].Add(cp) |> ignore
            cp <- np

    dm

let countRooms (dm: Dictionary<Point, HashSet<Point>>) minDoors =
    let visited = new Dictionary<Point, int>()
    let queue = new Queue<Point>()
    let start = { x = 0; y = 0 }
    visited.[start] <- 0
    queue.Enqueue(start)
    let mutable roomCount = 0

    while queue.Count > 0 do
        let p = queue.Dequeue()
        for np in dm.[p] do
            if not (visited.ContainsKey np) then
                visited.[np] <- visited.[p] + 1
                if visited.[np] >= minDoors then
                    roomCount <- roomCount + 1
                queue.Enqueue(np)

    roomCount

[<EntryPoint>]
let main argv =
    let regex = File.ReadAllText("input.txt")
    let dm = buildMap regex
    let rooms = countRooms dm 1000
    printfn "%d" rooms
    0
