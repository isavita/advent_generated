open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let distances =
        lines
        |> Array.mapi (fun i line ->
            let nums = 
                Regex.Matches(line, "-?\d+")
                |> Seq.map (fun m -> int m.Value)
                |> Seq.toArray
            let d = abs nums.[6] + abs nums.[7] + abs nums.[8]
            (i, d))
    let (idx, _) = Array.minBy snd distances
    printfn "%d" idx
    0