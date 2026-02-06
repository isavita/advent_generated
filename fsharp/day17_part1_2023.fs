
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let g = File.ReadAllLines "input.txt" |> Array.filter (fun s -> s.Length > 0) |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - 48))
    let h, w = g.Length, g.[0].Length
    let dists = Array.create (h * w * 20) Int32.MaxValue
    let pq = PriorityQueue<int * int * int * int * int * int, int>()
    let inline getI y x d s = (y * w + x) * 20 + d * 4 + s
    let getD dx dy = if dx = 0 then (if dy = -1 then 0 else 2) else (if dx = -1 then 1 else 3)
    
    pq.Enqueue((0, 0, 0, 0, 0, 0), 0)
    dists.[getI 0 0 4 0] <- 0
    
    let mutable res = 0
    let mutable state = (0, 0, 0, 0, 0, 0)
    let mutable p = 0
    let dirs = [| (0, 1); (1, 0); (0, -1); (-1, 0) |]
    
    while pq.TryDequeue(&state, &p) do
        let x, y, dx, dy, cnt, cost = state
        let dIdx = if dx = 0 && dy = 0 then 4 else getD dx dy
        if cost <= dists.[getI y x dIdx cnt] then
            if x = w - 1 && y = h - 1 then
                res <- cost
                pq.Clear()
            else
                for i = 0 to 3 do
                    let ndx, ndy = dirs.[i]
                    let ncnt = if ndx = dx && ndy = dy then cnt + 1 else 1
                    if (ndx <> -dx || ndy <> -dy || (dx = 0 && dy = 0)) && ncnt <= 3 then
                        let nx, ny = x + ndx, y + ndy
                        if nx >= 0 && nx < w && ny >= 0 && ny < h then
                            let ncost = cost + g.[ny].[nx]
                            let ni = getD ndx ndy
                            if ncost < dists.[getI ny nx ni ncnt] then
                                dists.[getI ny nx ni ncnt] <- ncost
                                pq.Enqueue((nx, ny, ndx, ndy, ncnt, ncost), ncost + (w - 1 - nx) + (h - 1 - ny))
    printfn "%d" res
    0

