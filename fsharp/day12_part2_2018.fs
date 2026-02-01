
open System
open System.Collections.Generic
open System.IO

let lines = File.ReadAllLines "input.txt"
let init = lines.[0].Substring(15).Trim()
let rules = Dictionary<string,bool>()
for i in 1..lines.Length-1 do
    let line = lines.[i]
    if line.Contains "=>" then
        let p = line.Split([|"=>"|], StringSplitOptions.RemoveEmptyEntries)
        rules.[p.[0].Trim()] <- p.[1].Trim() = "#"

let plants = HashSet<int>()
for i in 0..init.Length-1 do
    if init.[i] = '#' then plants.Add i |> ignore

let target = 50000000000L
let mutable prevPat = ""
let mutable prevSum = 0L
let sb = Text.StringBuilder 128
for gen in 0L..target-1L do
    let min = if plants.Count=0 then 0 else Seq.min plants
    let max = if plants.Count=0 then 0 else Seq.max plants
    let next = HashSet<int>()
    for i in min-2..max+2 do
        sb.Clear() |> ignore
        for j in i-2..i+2 do sb.Append(if plants.Contains j then '#' else '.') |> ignore
        let pat = sb.ToString()
        match rules.TryGetValue pat with
        | true, true -> next.Add i |> ignore
        | _ -> ()
    plants.Clear()
    for x in next do plants.Add x |> ignore
    let min = if plants.Count=0 then 0 else Seq.min plants
    let max = if plants.Count=0 then 0 else Seq.max plants
    sb.Clear() |> ignore
    for i in min..max do sb.Append(if plants.Contains i then '#' else '.') |> ignore
    let curPat = sb.ToString()
    let curSum = plants |> Seq.map int64 |> Seq.sum
    if prevPat <> "" && curPat = prevPat then
        let offset = curSum - prevSum
        printfn "%d" (curSum + offset * (target - gen - 1L))
        exit 0
    prevPat <- curPat
    prevSum <- curSum
printfn "%d" (plants |> Seq.map int64 |> Seq.sum)
