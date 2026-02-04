
open System
open System.IO
open System.Collections.Generic

let read () =
    let d = Dictionary<string, Dictionary<string, int>>()
    for line in File.ReadLines "input.txt" do
        let p = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        if p.Length = 5 then
            let a = p.[0]
            let b = p.[2]
            let v = Int32.Parse p.[4]
            if not (d.ContainsKey a) then d.[a] <- Dictionary()
            d.[a].[b] <- v
            if not (d.ContainsKey b) then d.[b] <- Dictionary()
            d.[b].[a] <- v
    d

let allLocations (d: Dictionary<string, Dictionary<string, int>>) =
    let s = HashSet<string>()
    for kv in d do
        s.Add kv.Key |> ignore
        for k in kv.Value.Keys do s.Add k |> ignore
    s |> Seq.toArray

let distance (route: string[]) (d: Dictionary<string, Dictionary<string, int>>) =
    let mutable sum = 0
    for i = 0 to route.Length - 2 do
        sum <- sum + d.[route.[i]].[route.[i+1]]
    sum

let swap (a: string[]) i j =
    let t = a.[i]
    a.[i] <- a.[j]
    a.[j] <- t

let rec permute (arr: string[]) i (maxDist: int ref) (d: Dictionary<string, Dictionary<string, int>>) =
    if i = arr.Length then
        let cur = distance arr d
        if cur > !maxDist then maxDist := cur
    else
        for j = i to arr.Length - 1 do
            swap arr i j
            permute arr (i+1) maxDist d
            swap arr i j

[<EntryPoint>]
let main argv =
    let distances = read ()
    let locs = allLocations distances
    let best = ref 0
    permute locs 0 best distances
    printfn "%d" !best
    0
