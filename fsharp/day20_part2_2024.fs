
open System
open System.IO
open System.Collections.Generic

let MAX_H,MAX_W,MAX_STEPS = 1005,1005,20
let INF = -1
let dr,dc = [|1;-1;0;0|],[|0;0;1;-1|]

let grid = Array.init MAX_H (fun _ -> Array.zeroCreate<char> MAX_W)
let walls = Array.init MAX_H (fun _ -> Array.zeroCreate<int> MAX_W)
let dist_s = Array.init MAX_H (fun _ -> Array.zeroCreate<int> MAX_W)
let dist_e = Array.init MAX_H (fun _ -> Array.zeroCreate<int> MAX_W)
let dist_c = Array.init MAX_H (fun _ -> Array.zeroCreate<int> MAX_W)

let mutable H,W = 0,0
let mutable S = (0,0)
let mutable E = (0,0)

let inline isValid r c = r >= 0 && r < H && c >= 0 && c < W && walls.[r].[c] = 0

let bfs start (dist:int[][]) =
    for i in 0..H-1 do for j in 0..W-1 do dist.[i].[j] <- INF
    let q = Queue<int*int>()
    let r,c = start
    if isValid r c then
        dist.[r].[c] <- 0
        q.Enqueue(start)
        while q.Count > 0 do
            let r,c = q.Dequeue()
            let d = dist.[r].[c]
            for i in 0..3 do
                let nr,nc = r+dr.[i],c+dc.[i]
                if isValid nr nc && dist.[nr].[nc] = INF then
                    dist.[nr].[nc] <- d + 1
                    q.Enqueue((nr,nc))

let limitedBfs (sr,sc) maxSteps =
    for i in 0..H-1 do for j in 0..W-1 do dist_c.[i].[j] <- INF
    let q = Queue<int*int>()
    dist_c.[sr].[sc] <- 0
    q.Enqueue((sr,sc))
    while q.Count > 0 do
        let r,c = q.Dequeue()
        let d = dist_c.[r].[c]
        if d < maxSteps then
            for i in 0..3 do
                let nr,nc = r+dr.[i],c+dc.[i]
                if nr >= 0 && nr < H && nc >= 0 && nc < W && dist_c.[nr].[nc] = INF then
                    dist_c.[nr].[nc] <- d + 1
                    q.Enqueue((nr,nc))

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    H <- lines.Length
    W <- lines.[0].Length
    for i in 0..H-1 do
        for j in 0..W-1 do
            grid.[i].[j] <- lines.[i].[j]
            if grid.[i].[j] = 'S' then S <- (i,j)
            elif grid.[i].[j] = 'E' then E <- (i,j)
            walls.[i].[j] <- if grid.[i].[j] = '#' then 1 else 0
    bfs S dist_s
    bfs E dist_e
    let normalCost = dist_s.[fst E].[snd E]
    if normalCost = INF then printfn "0"; 0
    else
        let mutable cheatCount = 0L
        for sr in 0..H-1 do
            for sc in 0..W-1 do
                if walls.[sr].[sc] = 0 && dist_s.[sr].[sc] <> INF then
                    let sd = dist_s.[sr].[sc]
                    limitedBfs (sr,sc) MAX_STEPS
                    for er in 0..H-1 do
                        for ec in 0..W-1 do
                            if walls.[er].[ec] = 0 && dist_e.[er].[ec] <> INF then
                                let s = dist_c.[er].[ec]
                                if s > 0 && s <= MAX_STEPS then
                                    let ed = dist_e.[er].[ec]
                                    let totalCost = sd + s + ed
                                    if totalCost < normalCost && normalCost - totalCost >= 100 then
                                        cheatCount <- cheatCount + 1L
        printfn "%d" cheatCount
        0
