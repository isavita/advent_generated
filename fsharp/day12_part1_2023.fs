
open System
open System.IO
open System.Collections.Generic

type Row = { Springs: string; Group: int list }

let parse (lines: string[]) =
    lines
    |> Array.map (fun l ->
        let p = l.Split(' ')
        { Springs = p.[0]
          Group = p.[1].Split(',') |> Array.map int |> Array.toList })

let count row =
    let n = row.Springs.Length
    let cache = Dictionary<(int*int*int), int64>()
    let rec go iSpr iGrp iDam =
        if iSpr = n then
            if iGrp = row.Group.Length && iDam = 0 then 1L
            elif iGrp = row.Group.Length - 1 && iDam = row.Group.[iGrp] then 1L
            else 0L
        else
            let key = (iSpr, iGrp, iDam)
            match cache.TryGetValue(key) with
            | true, v -> v
            | _ ->
                let c = row.Springs.[iSpr]
                let mutable res = 0L
                if c = '.' || c = '?' then
                    if iDam = 0 then res <- res + go (iSpr+1) iGrp iDam
                    elif iDam = row.Group.[iGrp] then res <- res + go (iSpr+1) (iGrp+1) 0
                if c = '#' || c = '?' then
                    if iGrp < row.Group.Length && iDam < row.Group.[iGrp] then
                        res <- res + go (iSpr+1) iGrp (iDam+1)
                cache.[key] <- res
                res
    go 0 0 0

[<EntryPoint>]
let main _ =
    let rows = File.ReadAllLines "input.txt" |> parse
    rows
    |> Array.map count
    |> Array.sum
    |> printfn "%d"
    0
