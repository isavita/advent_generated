
open System
open System.IO

[<EntryPoint>]
let main _ =
    let n = 100
    let mutable grid = Array2D.create n n 0
    let mutable temp = Array2D.create n n 0
    File.ReadAllLines("input.txt")
    |> Array.iteri (fun i line ->
        line.ToCharArray()
        |> Array.iteri (fun j c -> grid.[i,j] <- if c = '#' then 1 else 0))
    let offsets = [| -1; 0; 1 |]
    for _ = 1 to 100 do
        for i = 0 to n-1 do
            for j = 0 to n-1 do
                let mutable cnt = 0
                for dx in offsets do
                    let x = i + dx
                    if x >= 0 && x < n then
                        for dy in offsets do
                            if dx <> 0 || dy <> 0 then
                                let y = j + dy
                                if y >= 0 && y < n then cnt <- cnt + grid.[x,y]
                temp.[i,j] <-
                    if grid.[i,j] = 1 then
                        if cnt = 2 || cnt = 3 then 1 else 0
                    else
                        if cnt = 3 then 1 else 0
        let swap = grid
        grid <- temp
        temp <- swap
    grid
    |> Seq.cast<int>
    |> Seq.sum
    |> printfn "%d"
    0
