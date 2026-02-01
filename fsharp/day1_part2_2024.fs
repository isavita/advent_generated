
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines "input.txt"

    let pairs =
        lines
        |> Array.choose (fun line ->
            if String.IsNullOrWhiteSpace line then None
            else
                let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                if parts.Length >= 2 then
                    let left = Int32.Parse(parts.[0])
                    let right = Int32.Parse(parts.[1])
                    Some (left, right)
                else None)

    let lefts = pairs |> Array.map fst
    let rights = pairs |> Array.map snd

    let countDict = Dictionary<int, int>()
    for r in rights do
        if countDict.ContainsKey r then
            countDict.[r] <- countDict.[r] + 1
        else
            countDict.Add(r, 1)

    let similarityScore =
        lefts
        |> Array.fold (fun acc v ->
            let mutable cnt = 0
            if countDict.TryGetValue(v, &cnt) then
                acc + int64 v * int64 cnt
            else
                acc) 0L

    printfn "%d" similarityScore
    0
