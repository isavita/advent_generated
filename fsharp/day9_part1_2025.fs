
open System
open System.IO

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let pts =
        lines |> Array.map (fun l ->
            let a = l.Split(',')
            int a.[0], int a.[1])

    let mutable best = 0L
    for i in 0 .. pts.Length - 1 do
        let x1, y1 = pts.[i]
        for j in i .. pts.Length - 1 do
            let dx = abs (x1 - fst pts.[j]) + 1 |> int64
            let dy = abs (y1 - snd pts.[j]) + 1 |> int64
            let area = dx * dy
            if area > best then best <- area
    printfn "%d" best
    0
