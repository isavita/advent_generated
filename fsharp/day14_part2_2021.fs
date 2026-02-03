
open System
open System.IO
open System.Collections.Generic

let get (d:Dictionary<_,int64>) k =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> 0L

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let template = lines.[0]

    let rules =
        lines.[1..]
        |> Array.choose (fun l ->
            if String.IsNullOrWhiteSpace l then None
            else
                let p = l.Split([|" -> "|], StringSplitOptions.None)
                Some (p.[0], p.[1]))
        |> dict

    let pairCounts = Dictionary<string,int64>()
    for i = 0 to template.Length - 2 do
        let p = template.Substring(i,2)
        pairCounts.[p] <- get pairCounts p + 1L

    for _ = 1 to 40 do
        let next = Dictionary<string,int64>()
        for kv in pairCounts do
            let pair = kv.Key
            let cnt  = kv.Value
            match rules.TryGetValue(pair) with
            | true, ins ->
                let left  = string pair.[0] + ins
                let right = ins + string pair.[1]
                next.[left]  <- get next left  + cnt
                next.[right] <- get next right + cnt
            | _ ->
                next.[pair] <- get next pair + cnt
        pairCounts.Clear()
        for kv in next do pairCounts.Add(kv.Key, kv.Value)

    let elemCounts = Dictionary<char,int64>()
    for kv in pairCounts do
        let ch = kv.Key.[0]
        elemCounts.[ch] <- get elemCounts ch + kv.Value
    let lastCh = template.[template.Length-1]
    elemCounts.[lastCh] <- get elemCounts lastCh + 1L

    let maxC = elemCounts.Values |> Seq.max
    let minC = elemCounts.Values |> Seq.min
    printfn "%d" (maxC - minC)
    0
