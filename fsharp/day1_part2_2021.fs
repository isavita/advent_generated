
open System
open System.IO
open System.Linq

[<EntryPoint>]
let main argv =
    let vals = File.ReadAllLines("input.txt")
               |> Seq.filter (not << String.IsNullOrWhiteSpace)
               |> Seq.map int
               |> Seq.toList

    let count = vals
                |> Seq.windowed 3
                |> Seq.map Seq.sum
                |> Seq.pairwise
                |> Seq.filter (fun (a, b) -> b > a)
                |> Seq.length

    printfn "%d" count
    0
