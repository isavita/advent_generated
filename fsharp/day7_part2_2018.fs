
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt" |> Array.filter (fun s -> s.Trim() <> "")
    let deps = Dictionary<char,char list>()
    let steps = HashSet<char>()
    for l in lines do
        let a,b = l.[5], l.[36]
        steps.Add a |> ignore
        steps.Add b |> ignore
        if not (deps.ContainsKey a) then deps.[a] <- []
        if not (deps.ContainsKey b) then deps.[b] <- []
        deps.[b] <- a::deps.[b]

    let all = steps |> Seq.toList
    let completed = HashSet<char>()
    let inProgress = HashSet<char>()
    let available = SortedSet<char>(all |> List.filter (fun s -> deps.[s].IsEmpty))
    let remain = Array.zeroCreate 5
    let cur = Array.zeroCreate 5
    let mutable time = 0

    while completed.Count < all.Length do
        for i in 0..4 do
            if remain.[i] = 0 && available.Count > 0 then
                let next = available.Min
                available.Remove next |> ignore
                cur.[i] <- next
                inProgress.Add next |> ignore
                remain.[i] <- int next - int 'A' + 61

        let min = remain |> Array.filter (fun x -> x > 0) |> Array.min
        time <- time + min
        for i in 0..4 do
            if remain.[i] > 0 then
                remain.[i] <- remain.[i] - min
                if remain.[i] = 0 then
                    let finished = cur.[i]
                    completed.Add finished |> ignore
                    inProgress.Remove finished |> ignore
                    for step in all do
                        if not (completed.Contains step) && not (inProgress.Contains step) then
                            if deps.[step] |> List.forall (fun p -> completed.Contains p) then
                                available.Add step |> ignore
                    cur.[i] <- char 0
    printfn "%d" time
    0
