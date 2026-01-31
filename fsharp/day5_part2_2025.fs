
open System
open System.IO
open System.Text.RegularExpressions

type Range = { Min: int64; Max: int64 }

let parse (s: string) =
    let m = Regex.Match(s.Trim(), @"^(\d+)-(\d+)$")
    if not m.Success then None else
    let a, b = int64 m.Groups.[1].Value, int64 m.Groups.[2].Value
    Some (min a b, max a b)

[<EntryPoint>]
let main _ =
    let ranges =
        File.ReadAllLines "input.txt"
        |> Array.choose parse
        |> Array.sort
        |> Array.fold (fun acc (l, h) ->
            match acc with
            | [] -> [{ Min = l; Max = h }]
            | last::tl when l <= last.Max ->
                { last with Max = max last.Max h }::tl
            | _ -> { Min = l; Max = h }::acc) []
        |> List.rev
    let total = ranges |> List.sumBy (fun r -> r.Max - r.Min + 1L)
    printfn "Total fresh IDs: %d" total
    0
