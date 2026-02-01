
open System
open System.IO
open System.Collections.Generic

type Pos = { r:int; c:int }
type Node = { p:Pos; h:int }

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let nr = lines.Length
    let nc = if nr = 0 then 0 else lines.[0].Length
    let grid = Array2D.init nr nc (fun r c -> int lines.[r].[c] - int '0')
    let dirs = [| (1,0); (-1,0); (0,1); (0,-1) |]
    let trailheads =
        [ for r in 0 .. nr-1 do
            for c in 0 .. nc-1 do
                if grid.[r,c] = 0 then yield { r=r; c=c } ]
    let mutable sum = 0
    for th in trailheads do
        let reached = Array.create (nr*nc) false
        let visited = Array3D.create nr nc 10 false
        visited.[th.r, th.c, 0] <- true
        let q = Queue<Node>()
        q.Enqueue({ p=th; h=0 })
        while q.Count > 0 do
            let cur = q.Dequeue()
            if cur.h = 9 then
                reached.[cur.p.r*nc + cur.p.c] <- true
            else
                for (dr,dc) in dirs do
                    let r2 = cur.p.r + dr
                    let c2 = cur.p.c + dc
                    if r2 >= 0 && r2 < nr && c2 >= 0 && c2 < nc then
                        if grid.[r2,c2] = cur.h + 1 && not visited.[r2,c2,cur.h+1] then
                            visited.[r2,c2,cur.h+1] <- true
                            q.Enqueue({ p={ r=r2; c=c2 }; h=cur.h+1 })
        sum <- sum + (Array.sumBy (fun b -> if b then 1 else 0) reached)
    printfn "%d" sum
    0
