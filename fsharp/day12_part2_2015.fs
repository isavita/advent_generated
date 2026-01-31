
open System
open System.IO
open System.Text.RegularExpressions
open System.Text.Json

let part1 (s:string) =
    Regex.Matches(s, @"-?\d+")
    |> Seq.cast<Match>
    |> Seq.sumBy (fun m -> int64 m.Value)

let rec sumJson (e:JsonElement) (ignoreRed:bool) =
    match e.ValueKind with
    | JsonValueKind.Number -> e.GetInt64()
    | JsonValueKind.Array ->
        e.EnumerateArray()
        |> Seq.map (fun v -> sumJson v ignoreRed)
        |> Seq.sum
    | JsonValueKind.Object ->
        if ignoreRed then
            let hasRed = e.EnumerateObject() |> Seq.exists (fun p -> p.Value.ValueKind = JsonValueKind.String && p.Value.GetString() = "red")
            if hasRed then 0L else
                e.EnumerateObject()
                |> Seq.sumBy (fun p -> sumJson p.Value ignoreRed)
        else
            e.EnumerateObject()
            |> Seq.sumBy (fun p -> sumJson p.Value ignoreRed)
    | _ -> 0L

[<EntryPoint>]
let main _ =
    let txt = File.ReadAllText "input.txt"
    let sum1 = part1 txt
    use doc = JsonDocument.Parse(txt)
    let sum2 = sumJson doc.RootElement true
    printfn "Part 1 Sum: %d" sum1
    printfn "Part 2 Sum: %d" sum2
    0
