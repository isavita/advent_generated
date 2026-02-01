
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt") |> Array.filter (fun l -> l <> "")
    let height = lines.Length
    if height = 0 then printfn "0"; 0 else
    let width = lines.[0].Length
    let mutable startX = -1
    let mutable startY = -1
    for y in 0 .. height-1 do
        let idx = lines.[y].IndexOf('S')
        if idx <> -1 then
            startX <- idx
            startY <- y
    if startX = -1 then 1
    else
        let mutable cur = Dictionary<int, uint64>()
        cur.[startX] <- 1UL
        for y in startY .. height-1 do
            let next = Dictionary<int, uint64>()
            for kvp in cur do
                let x = kvp.Key
                let cnt = kvp.Value
                let isSplitter = x >= 0 && x < width && lines.[y].[x] = '^'
                if isSplitter then
                    for nx in [x-1; x+1] do
                        match next.TryGetValue(nx) with
                        | true, v -> next.[nx] <- v + cnt
                        | _ -> next.[nx] <- cnt
                else
                    match next.TryGetValue(x) with
                    | true, v -> next.[x] <- v + cnt
                    | _ -> next.[x] <- cnt
            cur <- next
        let total = cur.Values |> Seq.fold (+) 0UL
        printfn "%d" total
        0
