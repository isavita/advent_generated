
open System
open System.Collections.Generic

let favoriteNumber = 1362

let isWall x y =
    let n = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
    let rec bits acc m =
        if m = 0 then acc
        else bits (acc + (m &&& 1)) (m >>> 1)
    (bits 0 n) % 2 <> 0

let bfsMaxSteps maxSteps =
    let visited = HashSet<int>()
    let q = Queue<int>()
    let pack x y = (y <<< 16) ||| x
    let start = pack 1 1
    visited.Add(start) |> ignore
    q.Enqueue(start)
    let mutable steps = 0
    let deltas = [|1,0; -1,0; 0,1; 0,-1|]
    while q.Count > 0 && steps < maxSteps do
        let size = q.Count
        for _ in 1..size do
            let cur = q.Dequeue()
            let x, y = cur &&& 0xFFFF, cur >>> 16
            for dx, dy in deltas do
                let nx, ny = x + dx, y + dy
                if nx >= 0 && ny >= 0 then
                    let nxt = pack nx ny
                    if not (isWall nx ny) && visited.Add(nxt) then
                        q.Enqueue(nxt)
        steps <- steps + 1
    visited.Count

[<EntryPoint>]
let main _ =
    printfn "%d" (bfsMaxSteps 50)
    0
