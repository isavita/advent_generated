
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let raw = File.ReadAllLines "input.txt"
    let rows = raw.Length
    let cols = raw |> Array.map (fun s -> s.Length) |> Array.max
    let grid = raw |> Array.map (fun s -> s.PadRight(cols, ' ') |> Seq.toArray)

    let poiTmp = Array.create 10 (-1, -1)
    let mutable maxIdx = -1
    for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
            let ch = grid.[r].[c]
            if Char.IsDigit ch then
                let idx = int ch - int '0'
                poiTmp.[idx] <- (r, c)
                if idx > maxIdx then maxIdx <- idx
    let numPois = maxIdx + 1
    let pois = poiTmp.[0 .. maxIdx]

    let bfs (sr, sc) =
        let dist = Array.create numPois Int32.MaxValue
        let visited = Array2D.create rows cols false
        let q = Queue<(int * int * int)>()
        visited.[sr, sc] <- true
        q.Enqueue(sr, sc, 0)
        while q.Count > 0 do
            let (r, c, d) = q.Dequeue()
            let ch = grid.[r].[c]
            if Char.IsDigit ch then
                let idx = int ch - int '0'
                if dist.[idx] = Int32.MaxValue then dist.[idx] <- d
            for dr, dc in [| (-1,0); (1,0); (0,-1); (0,1) |] do
                let nr, nc = r + dr, c + dc
                if nr >= 0 && nr < rows && nc >= 0 && nc < cols &&
                   not visited.[nr, nc] && grid.[nr].[nc] <> '#' then
                    visited.[nr, nc] <- true
                    q.Enqueue(nr, nc, d + 1)
        dist

    let graph = Array.init numPois (fun i -> bfs pois.[i])

    let memo = Dictionary<(int * int), int>()
    let rec dp mask i =
        if mask = (1 <<< numPois) - 1 then 0
        else
            match memo.TryGetValue((mask, i)) with
            | true, v -> v
            | _ ->
                let mutable best = Int32.MaxValue
                for j = 0 to numPois - 1 do
                    if (mask &&& (1 <<< j)) = 0 && graph.[i].[j] <> Int32.MaxValue then
                        let cand = graph.[i].[j] + dp (mask ||| (1 <<< j)) j
                        if cand < best then best <- cand
                memo.[(mask, i)] <- best
                best

    let result = dp (1 <<< 0) 0
    printfn "%d" result
    0
