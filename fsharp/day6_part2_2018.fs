
open System
open System.IO

let manhattan (x1:int) (y1:int) (x2:int) (y2:int) = abs (x1 - x2) + abs (y1 - y2)

[<EntryPoint>]
let main _ =
    let coords =
        File.ReadAllLines "input.txt"
        |> Array.map (fun line ->
            let parts = line.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
            (int (parts.[0].Trim()), int (parts.[1].Trim())))

    let xs = coords |> Array.map fst
    let ys = coords |> Array.map snd
    let minX = xs |> Array.min
    let maxX = xs |> Array.max
    let minY = ys |> Array.min
    let maxY = ys |> Array.max

    let mutable safe = 0
    for x = minX to maxX do
        for y = minY to maxY do
            let total =
                coords
                |> Array.sumBy (fun (cx, cy) -> manhattan x y cx cy)
            if total < 10000 then safe <- safe + 1

    printfn "%d" safe
    0
