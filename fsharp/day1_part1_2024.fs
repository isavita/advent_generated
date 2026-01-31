
open System
open System.IO

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let left, right =
        lines
        |> Array.choose (fun l -> if System.String.IsNullOrWhiteSpace l then None else Some l)
        |> Array.map (fun l -> l.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> fun a -> a.[0], a.[1])
        |> Array.unzip
    Array.sortInPlace left
    Array.sortInPlace right
    let dist = Array.mapi2 (fun i l r -> abs (l - r)) left right |> Array.sumBy int64
    printfn "%d" dist
    0
