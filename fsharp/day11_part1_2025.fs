open System
open System.IO
open System.Collections.Generic

let nameToIdx = Dictionary<string,int>()
let edges = ResizeArray<ResizeArray<int>>()
let getIdx (name:string) =
    if nameToIdx.ContainsKey(name) then nameToIdx.[name]
    else
        let idx = edges.Count
        nameToIdx.Add(name, idx)
        edges.Add(ResizeArray<int>())
        idx

let lines = File.ReadAllLines("input.txt")
for line in lines do
    let parts = line.Split([|':'|], 2, StringSplitOptions.RemoveEmptyEntries)
    if parts.Length = 2 then
        let u = getIdx (parts.[0].Trim())
        for token in parts.[1].Split([|' ';'\t'|], StringSplitOptions.RemoveEmptyEntries) do
            let v = getIdx (token.Trim())
            edges.[u].Add(v)

let n = edges.Count
let memo = Array.create n -1
let rec dfs u target =
    if u = target then 1
    elif memo.[u] <> -1 then memo.[u]
    else
        let mutable total = 0
        for v in edges.[u] do total <- total + dfs v target
        memo.[u] <- total
        total

[<EntryPoint>]
let main _ =
    let start = getIdx "you"
    let finish = getIdx "out"
    printfn "%d" (dfs start finish)
    0