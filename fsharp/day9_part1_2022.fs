
open System
open System.IO

let abs (x:int) = if x < 0 then -x else x

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let tailVisited = System.Collections.Generic.HashSet<int*int>()
    let mutable hx, hy = 0, 0
    let mutable tx, ty = 0, 0
    tailVisited.Add((tx, ty)) |> ignore
    for line in lines do
        let dir, steps = line.[0], int (line.Substring(2))
        for _ in 1..steps do
            match dir with
            | 'R' -> hx <- hx + 1
            | 'L' -> hx <- hx - 1
            | 'U' -> hy <- hy + 1
            | 'D' -> hy <- hy - 1
            | _ -> ()
            if abs(hx - tx) > 1 || abs(hy - ty) > 1 then
                if hx <> tx && hy <> ty then
                    tx <- tx + (if hx > tx then 1 else -1)
                    ty <- ty + (if hy > ty then 1 else -1)
                else
                    if hx <> tx then tx <- tx + (if hx > tx then 1 else -1)
                    if hy <> ty then ty <- ty + (if hy > ty then 1 else -1)
            tailVisited.Add((tx, ty)) |> ignore
    printfn "%d" tailVisited.Count
    0
