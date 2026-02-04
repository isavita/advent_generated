
open System
open System.IO
open System.Collections.Generic

let dirs = [| (0, -1); (0, 1); (1, 0); (-1, 0) |]

let bfs (grid: char[][]) (rows: int) (cols: int) (sr: int) (sc: int) =
    let dist = Array.create 10 -1
    let q = Queue<(int * int * int)>()
    let visited = Array2D.create rows cols false
    q.Enqueue(sr, sc, 0)
    while q.Count > 0 do
        let r, c, d = q.Dequeue()
        if visited.[r, c] then () else
        visited.[r, c] <- true
        let ch = grid.[r].[c]
        if Char.IsDigit ch then
            let idx = int ch - int '0'
            dist.[idx] <- d
        for dr, dc in dirs do
            let nr, nc = r + dr, c + dc
            if nr >= 0 && nr < rows && nc >= 0 && nc < cols && not visited.[nr, nc] && grid.[nr].[nc] <> '#' then
                q.Enqueue(nr, nc, d + 1)
    dist

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let grid = lines |> Array.map (fun s -> s.ToCharArray())
    let rows, cols = grid.Length, grid.[0].Length
    let pos = Array.create 10 (-1, -1)
    for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
            let ch = grid.[r].[c]
            if Char.IsDigit ch then
                let idx = int ch - int '0'
                pos.[idx] <- (r, c)
    let present = [0..9] |> List.filter (fun i -> let pr, pc = pos.[i] in pr <> -1)
    if present.Length = 1 then 0 else
    let oldToNew = Dictionary<int, int>()
    let newToOld = Array.zeroCreate<int> present.Length
    present |> List.iteri (fun i oldIdx ->
        oldToNew.[oldIdx] <- i
        newToOld.[i] <- oldIdx)
    let k = present.Length
    let graph = Array.init k (fun _ -> ResizeArray<(int * int)>())
    for oldIdx in present do
        let sr, sc = pos.[oldIdx]
        let dists = bfs grid rows cols sr sc
        for targetIdx in present do
            if targetIdx <> oldIdx && dists.[targetIdx] >= 0 then
                let i = oldToNew.[oldIdx]
                let j = oldToNew.[targetIdx]
                graph.[i].Add(j, dists.[targetIdx])
    let fullMask = (1 <<< k) - 1
    let dp = Array2D.create (1 <<< k) k Int32.MaxValue
    dp.[1 <<< 0, 0] <- 0
    for mask = 0 to fullMask do
        for i = 0 to k - 1 do
            let cur = dp.[mask, i]
            if cur <> Int32.MaxValue then
                for (j, w) in graph.[i] do
                    if (mask &&& (1 <<< j)) = 0 then
                        let nMask = mask ||| (1 <<< j)
                        let nd = cur + w
                        if nd < dp.[nMask, j] then dp.[nMask, j] <- nd
    let mutable ans = Int32.MaxValue
    for i = 0 to k - 1 do
        let back = graph.[i] |> Seq.tryFind (fun (t, _) -> t = 0)
        match back with
        | Some (_, wBack) ->
            let total = dp.[fullMask, i] + wBack
            if total < ans then ans <- total
        | None -> ()
    printfn "%d" ans
    0
