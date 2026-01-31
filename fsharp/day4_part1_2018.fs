
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt") |> Array.filter (fun s -> s <> "") |> Array.sort
    let minutes = Dictionary<int, int[]>()
    let total = Dictionary<int, int>()
    let mutable curGuard = 0
    let mutable sleepStart = -1
    for line in lines do
        if line.Contains("Guard") then
            curGuard <- (line.Split([|'#'|], 2).[1].Split(' ') |> Array.head) |> int
            if not (minutes.ContainsKey curGuard) then minutes.[curGuard] <- Array.zeroCreate 60
            if not (total.ContainsKey curGuard) then total.[curGuard] <- 0
            sleepStart <- -1
        elif line.Contains("falls asleep") then
            sleepStart <- int (line.Substring(15,2))
        elif line.Contains("wakes up") then
            let wake = int (line.Substring(15,2))
            for m = sleepStart to wake-1 do
                minutes.[curGuard].[m] <- minutes.[curGuard].[m] + 1
                total.[curGuard] <- total.[curGuard] + 1
            sleepStart <- -1
    let sleepiestGuard =
        total
        |> Seq.maxBy (fun kv -> kv.Value)
        |> fun kv -> kv.Key
    let bestMinute =
        minutes.[sleepiestGuard]
        |> Array.mapi (fun i v -> i, v)
        |> Seq.maxBy snd
        |> fst
    printfn "%d" (sleepiestGuard * bestMinute)
    0
