
open System
open System.IO
open System.Collections.Generic

type Record = { Time: DateTime; Event: string }
type Guard(id:int) =
    member _.Id = id
    member val Minutes = Array.zeroCreate<int> 60 with get, set

[<EntryPoint>]
let main _ =
    let records =
        File.ReadAllLines "input.txt"
        |> Array.map (fun line ->
            let t = DateTime.ParseExact(line.[1..16], "yyyy-MM-dd HH:mm", null)
            { Time = t; Event = line.[19..] })
        |> Array.sortBy (fun r -> r.Time)

    let guards = Dictionary<int, Guard>()
    let mutable curGuard = Unchecked.defaultof<Guard>
    let mutable sleepStart = 0

    for r in records do
        match r.Event with
        | e when e.Contains "begins shift" ->
            let id = e.Split(' ').[1].[1..] |> int
            if not (guards.ContainsKey id) then guards.Add(id, Guard id)
            curGuard <- guards.[id]
        | e when e.Contains "falls asleep" ->
            sleepStart <- r.Time.Minute
        | _ -> // wakes up
            for m = sleepStart to r.Time.Minute - 1 do
                curGuard.Minutes.[m] <- curGuard.Minutes.[m] + 1

    let bestGuard, bestMin =
        guards.Values
        |> Seq.collect (fun g -> seq { for i in 0 .. 59 -> g, i, g.Minutes.[i] })
        |> Seq.maxBy (fun (_, _, cnt) -> cnt)
        |> fun (g, i, _) -> g, i

    printfn "%d" (bestGuard.Id * bestMin)
    0
