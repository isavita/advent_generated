
open System
open System.IO
open System.Collections.Generic

let lines = File.ReadAllLines "input.txt"

let idx = Dictionary<string,int>()
let names = ResizeArray<string>()
let adj = ResizeArray<ResizeArray<int>>()

let getIndex name =
    match idx.TryGetValue name with
    | true, i -> i
    | _ ->
        let i = names.Count
        idx.[name] <- i
        names.Add name
        adj.Add (ResizeArray())
        i

let addEdge a b =
    let i = getIndex a
    let j = getIndex b
    adj.[i].Add j
    adj.[j].Add i

for line in lines do
    let p = line.Split([|'-'|], StringSplitOptions.RemoveEmptyEntries)
    addEdge (p.[0].Trim()) (p.[1].Trim())

let startIdx = getIndex "start"
let endIdx = getIndex "end"

let visited = Array.zeroCreate<int> names.Count
let isSmall i = names.[i].ToCharArray() |> Array.forall Char.IsLower

let rec dfs cur doubleUsed =
    if cur = endIdx then 1
    else
        visited.[cur] <- visited.[cur] + 1
        let mutable sum = 0
        for nxt in adj.[cur] do
            if nxt = startIdx then ()
            elif isSmall nxt && visited.[nxt] > 0 then
                if doubleUsed then ()
                else sum <- sum + dfs nxt true
            else
                sum <- sum + dfs nxt doubleUsed
        visited.[cur] <- visited.[cur] - 1
        sum

[<EntryPoint>]
let main _ =
    printfn "%d" (dfs startIdx false)
    0
