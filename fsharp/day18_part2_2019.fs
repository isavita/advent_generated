
open System
open System.Collections.Generic
open System.IO

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt") |> Array.filter (fun s -> s.Length > 0)
    let grid = lines |> Array.map (fun s -> s.ToCharArray())
    let height = grid.Length
    let width = grid.[0].Length
    
    let mutable cx, cy = -1, -1
    let mutable foundAt = false
    for y in 1 .. height - 2 do
        for x in 1 .. width - 2 do
            if not foundAt && grid.[y].[x] = '@' then
                cx <- x; cy <- y; foundAt <- true

    grid.[cy-1].[cx-1] <- '@'; grid.[cy-1].[cx] <- '#'; grid.[cy-1].[cx+1] <- '@'
    grid.[cy].[cx-1]   <- '#'; grid.[cy].[cx]   <- '#'; grid.[cy].[cx+1]   <- '#'
    grid.[cy+1].[cx-1] <- '@'; grid.[cy+1].[cx] <- '#'; grid.[cy+1].[cx+1] <- '@'

    let keyNodes = Array.create 26 (-1, -1)
    let mutable existsMask = 0
    let mutable numKeys = 0
    let robotStarts = Array.create 4 (-1, -1)
    let mutable robotIdx = 0

    for y in 0 .. height - 1 do
        for x in 0 .. width - 1 do
            let cell = grid.[y].[x]
            if Char.IsLower(cell) then
                let id = int cell - int 'a'
                keyNodes.[id] <- (x, y)
                existsMask <- existsMask ||| (1 <<< id)
                numKeys <- max numKeys (id + 1)
            elif cell = '@' then
                if robotIdx < 4 then
                    robotStarts.[robotIdx] <- (x, y)
                    robotIdx <- robotIdx + 1

    let adj = Array.init (numKeys + 4) (fun _ -> List<int * int * int>())
    let dirs = [| (0, 1); (0, -1); (1, 0); (-1, 0) |]

    let runBFS startNodeId (startX, startY) =
        let q = Queue<int * int * int * int>()
        q.Enqueue((startX, startY, 0, 0))
        let visited = Array2D.create height width false
        visited.[startY, startX] <- true
        while q.Count > 0 do
            let x, y, dist, req = q.Dequeue()
            let cell = grid.[y].[x]
            if Char.IsLower(cell) && (int cell - int 'a') <> startNodeId then
                adj.[startNodeId].Add((int cell - int 'a', dist, req))
            
            for i in 0 .. 3 do
                let dx, dy = dirs.[i]
                let nx, ny = x + dx, y + dy
                if nx >= 0 && nx < width && ny >= 0 && ny < height && grid.[ny].[nx] <> '#' && not visited.[ny, nx] then
                    visited.[ny, nx] <- true
                    let nextCell = grid.[ny].[nx]
                    let nextReq = if Char.IsUpper(nextCell) then req ||| (1 <<< (int nextCell - int 'A')) else req
                    q.Enqueue((nx, ny, dist + 1, nextReq))

    for i in 0 .. 25 do
        if keyNodes.[i] <> (-1, -1) then runBFS i keyNodes.[i]
    for i in 0 .. 3 do
        runBFS (numKeys + i) robotStarts.[i]

    let pack (r: int[]) (mask: int) : int64 =
        (int64 mask) ||| (int64 r.[0] <<< 26) ||| (int64 r.[1] <<< 31) ||| (int64 r.[2] <<< 36) ||| (int64 r.[3] <<< 41)

    let pq = PriorityQueue<int64, int>()
    let dists = Dictionary<int64, int>()
    let startRobots = [| numKeys; numKeys + 1; numKeys + 2; numKeys + 3 |]
    let startState = pack startRobots 0
    dists.[startState] <- 0
    pq.Enqueue(startState, 0)

    let mutable result = -1
    let mutable finished = false
    let mutable state = 0L
    let mutable cost = 0
    while not finished && pq.TryDequeue(&state, &cost) do
        if cost <= dists.[state] then
            let mask = int (state &&& ((1L <<< 26) - 1L))
            if mask = existsMask then
                result <- cost
                finished <- true
            else
                let r0 = int ((state >>> 26) &&& 31L)
                let r1 = int ((state >>> 31) &&& 31L)
                let r2 = int ((state >>> 36) &&& 31L)
                let r3 = int ((state >>> 41) &&& 31L)
                let robots = [| r0; r1; r2; r3 |]
                for i in 0 .. 3 do
                    for (targetKey, d, req) in adj.[robots.[i]] do
                        if (mask &&& (1 <<< targetKey)) = 0 && (req &&& mask) = req then
                            let nextMask = mask ||| (1 <<< targetKey)
                            let nextRobots = Array.copy robots
                            nextRobots.[i] <- targetKey
                            let nextState = pack nextRobots nextMask
                            let nextCost = cost + d
                            if nextCost < dists.GetValueOrDefault(nextState, Int32.MaxValue) then
                                dists.[nextState] <- nextCost
                                pq.Enqueue(nextState, nextCost)

    printfn "%d" result
    0
