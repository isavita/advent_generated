
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    if lines.Length = 0 then 0 else
    let h, w = lines.Length, (lines |> Array.map (fun s -> s.Length) |> Array.max)
    let grid = lines |> Array.map (fun s -> s.PadRight(w).ToCharArray())
    let portals = Dictionary<string, (int * int) list>()
    for y in 0 .. h - 1 do
        for x in 0 .. w - 1 do
            if Char.IsUpper grid.[y].[x] then
                let findPos p1X p1Y p2X p2Y =
                    if p1X >= 0 && p1X < w && p1Y >= 0 && p1Y < h && grid.[p1Y].[p1X] = '.' then Some (p1X, p1Y)
                    elif p2X >= 0 && p2X < w && p2Y >= 0 && p2Y < h && grid.[p2Y].[p2X] = '.' then Some (p2X, p2Y)
                    else None
                if x + 1 < w && Char.IsUpper grid.[y].[x+1] then
                    match findPos (x-1) y (x+2) y with
                    | Some p -> let s = string grid.[y].[x] + string grid.[y].[x+1]
                                if not (portals.ContainsKey s) then portals.[s] <- []
                                if not (List.contains p portals.[s]) then portals.[s] <- p :: portals.[s]
                    | _ -> ()
                if y + 1 < h && Char.IsUpper grid.[y+1].[x] then
                    match findPos x (y-1) x (y+2) with
                    | Some p -> let s = string grid.[y].[x] + string grid.[y+1].[x]
                                if not (portals.ContainsKey s) then portals.[s] <- []
                                if not (List.contains p portals.[s]) then portals.[s] <- p :: portals.[s]
                    | _ -> ()
    let start, target = List.head portals.["AA"], List.head portals.["ZZ"]
    let jumps = Dictionary<int * int, (int * int * int)>()
    let isOut x y = x <= 2 || x >= w - 3 || y <= 2 || y >= h - 3
    for kvp in portals do
        if kvp.Key <> "AA" && kvp.Key <> "ZZ" then
            let p1, p2 = kvp.Value.[0], kvp.Value.[1]
            jumps.[p1] <- (fst p2, snd p2, if isOut (fst p1) (snd p1) then -1 else 1)
            jumps.[p2] <- (fst p1, snd p1, if isOut (fst p2) (snd p2) then -1 else 1)
    let q, v = Queue<int * int * int * int>(), Array3D.create 100 h w false
    q.Enqueue(fst start, snd start, 0, 0)
    v.[0, snd start, fst start] <- true
    let mutable ans = -1
    while q.Count > 0 && ans = -1 do
        let x, y, l, d = q.Dequeue()
        if (x, y, l) = (fst target, snd target, 0) then ans <- d
        else
            for dx, dy in [0,1; 0,-1; 1,0; -1,0] do
                let nx, ny = x + dx, y + dy
                if nx >= 0 && nx < w && ny >= 0 && ny < h && grid.[ny].[nx] = '.' && not v.[l, ny, nx] then
                    v.[l, ny, nx] <- true; q.Enqueue(nx, ny, l, d + 1)
            match jumps.TryGetValue((x, y)) with
            | true, (tx, ty, dl) when l + dl >= 0 && l + dl < 100 && not v.[l + dl, ty, tx] ->
                v.[l + dl, ty, tx] <- true; q.Enqueue(tx, ty, l + dl, d + 1)
            | _ -> ()
    printfn "%d" ans; 0
