
open System
open System.Collections.Generic
open System.IO

[<EntryPoint>]
let main _ =
    let adj = Dictionary<string, List<string>>()
    if File.Exists "input.txt" then
        for line in File.ReadLines "input.txt" do
            let parts = line.Split([|':'; ' '|], StringSplitOptions.RemoveEmptyEntries)
            if parts.Length > 1 then
                let u = parts.[0]
                if not (adj.ContainsKey u) then adj.[u] <- List<string>()
                for i in 1 .. parts.Length - 1 do
                    let v = parts.[i]
                    if not (adj.ContainsKey v) then adj.[v] <- List<string>()
                    adj.[u].Add(v)
                    adj.[v].Add(u)

    let nodes = adj.Keys |> Seq.toArray
    let n = nodes.Length
    if n > 1 then
        let nodeToIndex = Dictionary<string, int>()
        for i in 0 .. n - 1 do nodeToIndex.[nodes.[i]] <- i
        let g = Array.init n (fun i -> adj.[nodes.[i]] |> Seq.map (fun neighbor -> nodeToIndex.[neighbor]) |> Seq.toArray)

        let flow = Dictionary<int64, int>()
        let parent = Array.create n -1
        let q = Queue<int>()

        let getFlowValue u v =
            let key = (int64 u * int64 n) + int64 v
            let mutable f = 0
            if flow.TryGetValue(key, &f) then f else 0

        let solveFor t =
            flow.Clear()
            let mutable totalFlow = 0
            let mutable ok = true
            while ok && totalFlow < 4 do
                Array.Fill(parent, -1)
                q.Clear()
                q.Enqueue 0; parent.[0] <- 0
                let mutable found = false
                while q.Count > 0 && not found do
                    let u = q.Dequeue()
                    for v in g.[u] do
                        if parent.[v] = -1 then
                            let f = getFlowValue u v
                            if 1 - f > 0 then
                                parent.[v] <- u
                                if v = t then found <- true else q.Enqueue v
                if found then
                    totalFlow <- totalFlow + 1
                    let mutable curr = t
                    while curr <> 0 do
                        let prev = parent.[curr]
                        let k1 = (int64 prev * int64 n) + int64 curr
                        let k2 = (int64 curr * int64 n) + int64 prev
                        flow.[k1] <- (getFlowValue prev curr) + 1
                        flow.[k2] <- (getFlowValue curr prev) - 1
                        curr <- prev
                else ok <- false
            
            if totalFlow = 3 then
                let visited = HashSet<int>()
                q.Clear(); q.Enqueue 0; visited.Add 0 |> ignore
                while q.Count > 0 do
                    let u = q.Dequeue()
                    for v in g.[u] do
                        if 1 - (getFlowValue u v) > 0 && not (visited.Contains v) then
                            visited.Add v |> ignore; q.Enqueue v
                Some(int64 visited.Count * int64 (n - visited.Count))
            else None

        let mutable ans = 0L
        let mutable t = n - 1
        while ans = 0L && t >= 1 do
            match solveFor t with
            | Some res -> ans <- res
            | None -> t <- t - 1
        printfn "%d" ans
    0

