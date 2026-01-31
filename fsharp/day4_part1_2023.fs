
open System
open System.IO
open System.Text.RegularExpressions

let parseNumbers (s:string) =
    Regex.Matches(s, "\d+") |> Seq.map (fun m -> int m.Value) |> Set.ofSeq

let points (win:Set<int>) (have:Set<int>) =
    let m = Set.intersect win have |> Set.count
    if m = 0 then 0 else pown 2 (m - 1)

let score (line:string) =
    let [|win; have|] = line.Split([|':'|]).[1].Split([|'|'|])
    points (parseNumbers win) (parseNumbers have)

[<EntryPoint>]
let main _ =
    File.ReadAllLines "input.txt"
    |> Array.sumBy score
    |> printfn "%d"
    0
