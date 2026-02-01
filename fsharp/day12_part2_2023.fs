open System
open System.IO
open System.Collections.Generic

let countArrangements (springs:string, groups:int list) =
    let n = springs.Length
    let gLen = groups.Length
    let memo = Dictionary<(int*int*int), int64>()
    let rec dfs iSpr iGrp cont =
        let key = (iSpr,iGrp,cont)
        match memo.TryGetValue key with
        | true,v -> v
        | _ ->
            let res =
                if iSpr = n then
                    if iGrp = gLen && cont = 0 then 1L
                    elif iGrp = gLen-1 && cont = groups.[iGrp] then 1L
                    else 0L
                else
                    let c = springs.[iSpr]
                    let mutable r = 0L
                    if (c = '.' || c = '?') then
                        if cont = 0 then r <- r + dfs (iSpr+1) iGrp cont
                        elif iGrp < gLen && cont = groups.[iGrp] then r <- r + dfs (iSpr+1) (iGrp+1) 0
                    if (c = '#' || c = '?') then
                        if iGrp < gLen && cont < groups.[iGrp] then r <- r + dfs (iSpr+1) iGrp (cont+1)
                    r
            memo.[key] <- res
            res
    dfs 0 0 0

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let total =
        lines
        |> Array.fold (fun acc line ->
            let parts = line.Split(' ')
            let springs = parts.[0]
            let groups = parts.[1].Split(',') |> Array.map int |> Array.toList
            let unfoldedSprings = String.concat "?" (List.replicate 5 springs)
            let unfoldedGroups = List.concat (List.replicate 5 groups)
            acc + countArrangements (unfoldedSprings, unfoldedGroups)
        ) 0L
    printfn "%d" total
    0