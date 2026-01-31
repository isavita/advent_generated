
open System
open System.IO

let isSafe (levels: int[]) =
    if levels.Length < 2 then false else
    let d = levels[1] - levels[0]
    if d = 0 then false else
    let inc = d > 0
    levels
    |> Array.pairwise
    |> Array.forall (fun (a,b) ->
        let diff = b - a
        (if inc then diff > 0 else diff < 0) &&
        (abs diff) >= 1 && (abs diff) <= 3)

[<EntryPoint>]
let main _ =
    File.ReadAllLines "input.txt"
    |> Array.filter (fun s -> String.IsNullOrWhiteSpace s |> not)
    |> Array.map (fun l -> l.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int)
    |> Array.filter isSafe
    |> Array.length
    |> printfn "%d"
    0
