
open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let txt = File.ReadAllText "input.txt"
    Regex.Matches(txt, "mul\((\d+),(\d+)\)")
    |> Seq.sumBy (fun m -> int m.Groups.[1].Value * int m.Groups.[2].Value)
    |> printfn "%d"
    0
