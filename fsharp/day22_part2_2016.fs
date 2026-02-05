
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Point = { X:int; Y:int }
type Node = { Used:int; Avail:int }

let maxDim = 50
let wallThreshold = 400

let bfs (nodes:Node[,]) width height goal from toPos =
    if from = toPos then 0
    else
        let depth = Array2D.create width height -1
        let q = Queue<Point>()
        depth.[from.X, from.Y] <- 0
        q.Enqueue from
        let dirs = [|(0,1);(0,-1);(1,0);(-1,0)|]
        let mutable ans = -1
        while q.Count > 0 && ans = -1 do
            let p = q.Dequeue()
            if p = toPos then ans <- depth.[p.X, p.Y]
            else
                for (dx,dy) in dirs do
                    let nx = p.X + dx
                    let ny = p.Y + dy
                    if nx >= 0 && ny >= 0 && nx < width && ny < height then
                        if not (nx = goal.X && ny = goal.Y) &&
                           nodes.[nx,ny].Used <= wallThreshold &&
                           depth.[nx,ny] = -1 then
                            depth.[nx,ny] <- depth.[p.X,p.Y] + 1
                            q.Enqueue { X = nx; Y = ny }
        ans

let minMoves (nodes:Node[,]) width height =
    let mutable goal = { X = width-1; Y = 0 }
    let mutable hole = { X = -1; Y = -1 }
    for y = 0 to height-1 do
        for x = 0 to width-1 do
            if nodes.[x,y].Used = 0 then hole <- { X = x; Y = y }
    let mutable moves = 0
    while goal <> { X = 0; Y = 0 } do
        let target = { X = goal.X-1; Y = goal.Y }
        let d = bfs nodes width height goal hole target
        if d = -1 then -1 |> printfn "%d"; Environment.Exit 0
        moves <- moves + d + 1
        let tmp = goal
        goal <- target
        hole <- tmp
    moves

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    if lines.Length < 3 then 0 else
    let regex = Regex(@"\/dev\/grid\/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%")
    let mutable maxX = 0
    let mutable maxY = 0
    let mutable raw = List.empty
    for i = 2 to lines.Length-1 do
        let m = regex.Match(lines.[i])
        if m.Success then
            let x = int m.Groups.[1].Value
            let y = int m.Groups.[2].Value
            let used = int m.Groups.[4].Value
            let avail = int m.Groups.[5].Value
            raw <- (x,y,{ Used = used; Avail = avail }) :: raw
            if x > maxX then maxX <- x
            if y > maxY then maxY <- y
    let width = maxX + 1
    let height = maxY + 1
    let nodes = Array2D.create width height { Used = 0; Avail = 0 }
    for (x,y,node) in raw do nodes.[x,y] <- node
    let result = minMoves nodes width height
    printfn "%d" result
    0
