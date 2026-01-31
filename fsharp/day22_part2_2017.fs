
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let h = lines.Length
    let w = lines.[0].Length
    let startX = w / 2
    let startY = h / 2

    // key = (int64)x <<< 32 ||| (uint32)y
    let key (x:int) (y:int) = (int64 x <<< 32) ||| int64 (uint32 y)

    let grid = Dictionary<int64,int>()
    for y in 0 .. h-1 do
        let line = lines.[y]
        for x in 0 .. line.Length-1 do
            if line.[x] = '#' then
                grid.[key x y] <- 2   // Infected

    let dx = [| 0; 1; 0; -1 |]
    let dy = [| -1; 0; 1; 0 |]

    let mutable x = startX
    let mutable y = startY
    let mutable dir = 0
    let mutable infected = 0

    for _ in 1 .. 10_000_000 do
        let k = key x y
        let state = 
            match grid.TryGetValue k with
            | true, v -> v
            | _ -> 0   // Clean

        match state with
        | 0 ->                     // Clean
            dir <- (dir + 3) &&& 3
            grid.[k] <- 1           // Weakened
        | 1 ->                     // Weakened
            grid.[k] <- 2           // Infected
            infected <- infected + 1
        | 2 ->                     // Infected
            dir <- (dir + 1) &&& 3
            grid.[k] <- 3           // Flagged
        | _ ->                     // Flagged
            dir <- (dir + 2) &&& 3
            grid.Remove k |> ignore // Clean again

        x <- x + dx.[dir]
        y <- y + dy.[dir]

    printfn "%d" infected
    0
