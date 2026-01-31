
open System
open System.IO

let isSafe (levels: int[]) =
    if levels.Length < 2 then false else
    let diff = levels[1] - levels[0]
    if diff = 0 then false else
    let inc = diff > 0
    levels[..levels.Length-2]
    |> Array.forall (fun x ->
        let d = levels[Array.findIndex ((=) x) levels + 1] - x
        if inc then d > 0 && d <= 3 else d < 0 && d >= -3)

let isSafeDamp (levels: int[]) =
    if isSafe levels then true else
    [|0..levels.Length-1|]
    |> Array.exists (fun i ->
        let copy = Array.append levels[..i-1] levels[i+1..]
        isSafe copy)

[<EntryPoint>]
let main _ =
    File.ReadAllLines "input.txt"
    |> Array.filter (fun l -> l.Trim() <> "")
    |> Array.map (fun l -> l.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int)
    |> Array.filter isSafeDamp
    |> Array.length
    |> printfn "%d"
    0
