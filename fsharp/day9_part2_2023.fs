open System
open System.IO

let diff (a:int[]) = Array.init (a.Length - 1) (fun i -> a.[i + 1] - a.[i])

let rec collectFirsts (seq:int[]) (acc:list<int>) =
    if Array.forall ((=) 0) seq then acc
    else collectFirsts (diff seq) (seq.[0] :: acc)

let pastPrediction (history:int[]) =
    let firsts = collectFirsts history []
    let mutable p = 0
    for v in firsts do p <- v - p
    p

[<EntryPoint>]
let main _ =
    let total =
        File.ReadAllLines "input.txt"
        |> Array.map (fun l -> l.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int)
        |> Array.sumBy pastPrediction
    printfn "%d" total
    0