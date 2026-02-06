
open System
open System.Collections.Generic

let solve () =
    let lines = System.IO.File.ReadAllLines "input.txt"
    let ROWS = lines.Length
    let COLS = lines.[0].Length
    let grid = Array2D.init ROWS COLS (fun r c -> if c < lines.[r].Length then lines.[r].[c] else ' ')

    let energyCost = function 'A' -> 1 | 'B' -> 10 | 'C' -> 100 | 'D' -> 1000 | _ -> 0
    let roomCols = [| 3; 5; 7; 9 |]
    let targetChars = [| 'A'; 'B'; 'C'; 'D' |]
    let getTargetCol = function 'A' -> 3 | 'B' -> 5 | 'C' -> 7 | 'D' -> 9 | _ -> -1
    let isEntrance c = c = 3 || c = 5 || c = 7 || c = 9

    let isDone (g: char[,]) =
        let mutable ok = true
        for i in 0 .. 3 do
            let c = roomCols.[i]
            let target = targetChars.[i]
            for r in 2 .. ROWS - 2 do
                if g.[r, c] <> target then ok <- false
        ok

    let gridToString (g: char[,]) =
        let arr = Array.zeroCreate (ROWS * COLS)
        let mutable i = 0
        for r in 0 .. ROWS - 1 do
            for c in 0 .. COLS - 1 do
                arr.[i] <- g.[r, c]
                i <- i + 1
        String(arr)

    let pq = PriorityQueue<char[,] * int, int>()
    let dists = Dictionary<string, int>()

    let startStr = gridToString grid
    dists.[startStr] <- 0
    pq.Enqueue((grid, 0), 0)

    let mutable result = -1

    while pq.Count > 0 && result = -1 do
        let (currGrid, currEnergy) = pq.Dequeue()
        let currStr = gridToString currGrid
        
        let mutable prevDist = 0
        if (not (dists.TryGetValue(currStr, &prevDist)) || currEnergy <= prevDist) then
            if isDone currGrid then
                result <- currEnergy
            else
                for r in 1 .. ROWS - 2 do
                    for c in 1 .. COLS - 2 do
                        let amph = currGrid.[r, c]
                        if amph >= 'A' && amph <= 'D' then
                            let targetCol = getTargetCol amph
                            let mutable needsMove = false
                            if r = 1 then
                                needsMove <- true
                            elif c <> targetCol then
                                needsMove <- true
                            else
                                let mutable blocking = false
                                for rr in r + 1 .. ROWS - 2 do
                                    if currGrid.[rr, c] <> '.' && currGrid.[rr, c] <> amph then blocking <- true
                                needsMove <- blocking
                            
                            if needsMove then
                                let visited = Array2D.create ROWS COLS false
                                let q = Queue<int * int * int>()
                                q.Enqueue((r, c, 0))
                                visited.[r, c] <- true
                                
                                while q.Count > 0 do
                                    let (cr, cc, d) = q.Dequeue()
                                    if d > 0 then
                                        let canStop = 
                                            if cr = 1 then
                                                r <> 1 && not (isEntrance cc)
                                            else
                                                cc = targetCol &&
                                                (let mutable nf = true
                                                 for rr in 2 .. ROWS - 2 do
                                                     if currGrid.[rr, cc] <> '.' && currGrid.[rr, cc] <> amph then nf <- false
                                                 nf) &&
                                                (let mutable dp = true
                                                 for rr in cr + 1 .. ROWS - 2 do
                                                     if currGrid.[rr, cc] = '.' then dp <- false
                                                 dp)
                                        
                                        if canStop then
                                            let nextEnergy = currEnergy + d * energyCost amph
                                            let nextGrid = Array2D.copy currGrid
                                            nextGrid.[r, c] <- '.'
                                            nextGrid.[cr, cc] <- amph
                                            let s = gridToString nextGrid
                                            if not (dists.TryGetValue(s, &prevDist)) || nextEnergy < prevDist then
                                                dists.[s] <- nextEnergy
                                                pq.Enqueue((nextGrid, nextEnergy), nextEnergy)

                                    for (dr, dc) in [| (-1,0); (1,0); (0,-1); (0,1) |] do
                                        let nr, nc = cr + dr, cc + dc
                                        if nr >= 1 && nr < ROWS - 1 && nc >= 1 && nc < COLS - 1 && 
                                           currGrid.[nr, nc] = '.' && not visited.[nr, nc] then
                                            visited.[nr, nc] <- true
                                            q.Enqueue((nr, nc, d + 1))
    
    printfn "%d" result

[<EntryPoint>]
let main _ =
    solve()
    0
