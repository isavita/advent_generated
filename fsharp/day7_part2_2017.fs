
open System
open System.IO
open System.Collections.Generic

type Node = {
    weight:int
    mutable children:string list
    mutable total:int option
}

let lines = File.ReadAllLines("input.txt")
let nodes = Dictionary<string, Node>()
let childSet = HashSet<string>()

// first pass – create nodes and record children
for line in lines do
    let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let name = parts.[0]
    let w = parts.[1].Trim('(', ')')
    let weight = Int32.Parse w
    let children =
        if parts.Length > 2 then
            // parts.[2] = "->"
            parts.[3..]
            |> Array.map (fun s -> s.TrimEnd(','))
            |> List.ofArray
        else []
    nodes.[name] <- { weight = weight; children = children; total = None }
    for c in children do childSet.Add(c) |> ignore

// root is the only node that never appears as a child
let root =
    nodes.Keys
    |> Seq.find (fun k -> not (childSet.Contains(k)))

// depth‑first search that returns total weight and, if found, the corrected weight
let rec dfs (name:string) : int * int option =
    let node = nodes.[name]
    let childRes = node.children |> List.map dfs

    // propagate a found correction upward
    match childRes |> List.tryPick (fun (_,c) -> c) with
    | Some v -> (0, Some v)
    | None ->
        let childWeights = childRes |> List.map fst
        if childWeights.IsEmpty then
            (node.weight, None)
        else
            let freq = Dictionary<int,int>()
            for w in childWeights do
                freq.[w] <- freq.GetValueOrDefault(w,0) + 1

            if freq.Count = 1 then
                // balanced
                (node.weight + List.sum childWeights, None)
            else
                // one weight is different
                let unbalancedWeight =
                    freq |> Seq.find (fun kv -> kv.Value = 1) |> fun kv -> kv.Key
                let correctWeight =
                    freq |> Seq.find (fun kv -> kv.Value > 1) |> fun kv -> kv.Key
                let idx = childWeights |> List.findIndex ((=) unbalancedWeight)
                let badChild = node.children.[idx]
                let badNode = nodes.[badChild]
                let diff = correctWeight - unbalancedWeight
                let corrected = badNode.weight + diff
                (0, Some corrected)

// -------------------------------------------------------------
[<EntryPoint>]
let main _ =
    let (_, correction) = dfs root
    match correction with
    | Some v -> printfn "%d" v
    | None   -> ()
    0
