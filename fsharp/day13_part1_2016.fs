open System
open System.IO
open System.Collections.Generic
open System.Numerics

let isWall fav x y =
    let n = x*x + 3*x + 2*x*y + y + y*y + fav
    BitOperations.PopCount(uint32 n) % 2 = 1

let bfs fav start target =
    let visited = HashSet<(int*int)>()
    let q = Queue<(int*int)>()
    q.Enqueue(start)
    visited.Add(start) |> ignore
    let mutable steps = 0
    while q.Count > 0 do
        let size = q.Count
        for _ in 0 .. size-1 do
            let (x,y) = q.Dequeue()
            if (x,y) = target then
                printfn "%d" steps
                Environment.Exit 0
            for (dx,dy) in [ (1,0); (-1,0); (0,1); (0,-1) ] do
                let nx = x + dx
                let ny = y + dy
                if nx >= 0 && ny >= 0 && not (isWall fav nx ny) && visited.Add((nx,ny)) then
                    q.Enqueue((nx,ny))
        steps <- steps + 1

[<EntryPoint>]
let main _ =
    let fav = File.ReadAllText("input.txt").Trim() |> int
    bfs fav (1,1) (31,39)
    0