
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let mutable init = ""
    let rules = HashSet<string>()
    for line in lines do
        if line.StartsWith "initial state" then
            init <- line.Substring(line.IndexOf(": ") + 2)
        elif line.Contains "=>" then
            let parts = line.Split([| " => " |], StringSplitOptions.None)
            if parts.[1].[0] = '#' then rules.Add(parts.[0]) |> ignore
    let mutable state = 
        init
        |> Seq.mapi (fun i c -> i, c)
        |> Seq.filter (fun (_,c) -> c = '#')
        |> Seq.map fst
        |> Set.ofSeq
    for _ in 1 .. 20 do
        let minP = (state |> Seq.min)
        let maxP = (state |> Seq.max)
        let next =
            seq { for i = minP - 2 to maxP + 2 do
                  let pat = 
                      [| i-2; i-1; i; i+1; i+2 |]
                      |> Array.map (fun p -> if state.Contains p then '#' else '.')
                      |> String.Concat
                  if rules.Contains pat then yield i }
            |> Set.ofSeq
        state <- next
    let sum = state |> Seq.sum
    printfn "%d" sum
    0
