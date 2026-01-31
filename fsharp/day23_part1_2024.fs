
open System
open System.IO

let maxNodes = 2000
let nodeNames = Array.zeroCreate<string> maxNodes
let adj = Array2D.create maxNodes maxNodes false
let mutable nodeCount = 0
let nameToId = Collections.Generic.Dictionary<string,int>()

let getId (name:string) =
    match nameToId.TryGetValue name with
    | true, id -> id
    | false, _ ->
        if nodeCount >= maxNodes then failwith "too many nodes"
        nodeNames.[nodeCount] <- name
        nameToId.[name] <- nodeCount
        let out = nodeCount
        nodeCount <- nodeCount + 1
        out

File.ReadAllLines "input.txt"
|> Array.iter (fun (line:string) ->
    let p = line.IndexOf '-'
    if p > 0 && p < line.Length - 1 then
        let a = getId (line.Substring(0,p))
        let b = getId (line.Substring(p+1))
        adj.[a,b] <- true
        adj.[b,a] <- true)

let mutable cnt = 0
for i in 0..nodeCount-1 do
    for j in i+1..nodeCount-1 do
        if adj.[i,j] then
            for k in j+1..nodeCount-1 do
                if adj.[j,k] && adj.[k,i] &&
                   (nodeNames.[i].[0] = 't' || nodeNames.[j].[0] = 't' || nodeNames.[k].[0] = 't') then
                    cnt <- cnt + 1
printfn "%d" cnt
