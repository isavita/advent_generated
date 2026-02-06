
open System
open System.IO
open System.Collections.Generic

let rollSums = [ (3, 1L); (4, 3L); (5, 6L); (6, 7L); (7, 6L); (8, 3L); (9, 1L) ]
let cache = Dictionary<struct (int * int * int * int * int), int64 * int64>()

let rec solve p1 p1s p2 p2s turn =
    let state = struct (p1, p1s, p2, p2s, turn)
    let mutable res = (0L, 0L)
    if cache.TryGetValue(state, &res) then res
    else
        let mutable w1, w2 = 0L, 0L
        if turn = 0 then
            for (s, c) in rollSums do
                let np = (p1 + s - 1) % 10 + 1
                let ns = p1s + np
                if ns >= 21 then 
                    w1 <- w1 + c
                else
                    let (rw1, rw2) = solve np ns p2 p2s 1
                    w1 <- w1 + rw1 * c
                    w2 <- w2 + rw2 * c
        else
            for (s, c) in rollSums do
                let np = (p2 + s - 1) % 10 + 1
                let ns = p2s + np
                if ns >= 21 then 
                    w2 <- w2 + c
                else
                    let (rw1, rw2) = solve p1 p1s np ns 0
                    w1 <- w1 + rw1 * c
                    w2 <- w2 + rw2 * c
        cache.[state] <- (w1, w2)
        (w1, w2)

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let p1 = int (lines.[0].Split(':').[1].Trim())
    let p2 = int (lines.[1].Split(':').[1].Trim())
    let (w1, w2) = solve p1 0 p2 0 0
    printfn "%d" (if w1 > w2 then w1 else w2)
    0

