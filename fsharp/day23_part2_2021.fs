
open System
open System.Collections.Generic
open System.IO

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines "input.txt"
    let r0 = String [| input.[2].[3]; 'D'; 'D'; input.[3].[3] |]
    let r1 = String [| input.[2].[5]; 'C'; 'B'; input.[3].[5] |]
    let r2 = String [| input.[2].[7]; 'B'; 'A'; input.[3].[7] |]
    let r3 = String [| input.[2].[9]; 'A'; 'C'; input.[3].[9] |]
    let start = "..........." + r0 + r1 + r2 + r3
    let target = "...........AAAABBBBCCCCDDDD"
    let costs, roomCols, stops = [| 1; 10; 100; 1000 |], [| 2; 4; 6; 8 |], [| 0; 1; 3; 5; 7; 9; 10 |]
    let pq = PriorityQueue<string, int>()
    let dists = Dictionary<string, int>()
    pq.Enqueue(start, 0)
    dists.[start] <- 0
    let mutable ans = -1
    let mutable s = ""
    let mutable d = 0
    while pq.TryDequeue(&s, &d) && ans = -1 do
        let mutable vd = 0
        if dists.TryGetValue(s, &vd) && d <= vd then
            if s = target then ans <- d
            else
                let hStr = s.Substring(0, 11)
                let isPathClear h1 h2 =
                    let mutable ok = true
                    for i in (min h1 h2) .. (max h1 h2) do
                        if i <> h1 && hStr.[i] <> '.' then ok <- false
                    ok
                for h in 0..10 do
                    let pod = hStr.[h]
                    if pod <> '.' then
                        let rIdx = int pod - int 'A'
                        let rPos = 11 + rIdx * 4
                        let room = s.Substring(rPos, 4)
                        let mutable clean = true
                        for i in 0..3 do if room.[i] <> '.' && int room.[i] - int 'A' <> rIdx then clean <- false
                        if clean && isPathClear h roomCols.[rIdx] then
                            let depth = room.LastIndexOf '.'
                            if depth <> -1 then
                                let mutable ns = s.ToCharArray()
                                ns.[h] <- '.'; ns.[rPos + depth] <- pod
                                let nState, nDist = String ns, d + (abs(h - roomCols.[rIdx]) + depth + 1) * costs.[rIdx]
                                if not (dists.TryGetValue(nState, &vd)) || nDist < vd then
                                    dists.[nState] <- nDist; pq.Enqueue(nState, nDist)
                for rIdx in 0..3 do
                    let rPos = 11 + rIdx * 4
                    let mutable top = -1
                    for i in 0..3 do if top = -1 && s.[rPos+i] <> '.' then top <- i
                    if top <> -1 then
                        let pod = s.[rPos+top]
                        let mutable needs = false
                        for i in top..3 do if int s.[rPos+i] - int 'A' <> rIdx then needs <- true
                        if needs then
                            for h in stops do
                                if hStr.[h] = '.' && isPathClear roomCols.[rIdx] h then
                                    let mutable ns = s.ToCharArray()
                                    ns.[rPos+top] <- '.'; ns.[h] <- pod
                                    let nState, nDist = String ns, d + (abs(h - roomCols.[rIdx]) + top + 1) * costs.[int pod - int 'A']
                                    if not (dists.TryGetValue(nState, &vd)) || nDist < vd then
                                        dists.[nState] <- nDist; pq.Enqueue(nState, nDist)
    printfn "%d" ans
    0

