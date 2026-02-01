
open System
open System.IO
open System.Collections.Generic

let [<Literal>] FILENAME = "input.txt"
let [<Literal>] MAX_NODES = 2048

let nodeNames = Array.zeroCreate<string> MAX_NODES
let mutable nodeCount = 0
let adj = Array2D.zeroCreate<bool> MAX_NODES MAX_NODES
let maxCliqueNodes = ResizeArray<int>()

let getNodeId (name: string) =
    let rec find i =
        if i < nodeCount then
            if nodeNames.[i] = name then i
            else find (i + 1)
        else
            nodeNames.[nodeCount] <- name
            let id = nodeCount
            nodeCount <- nodeCount + 1
            id
    find 0

let findMaxCliqueRecursive (r: ResizeArray<int>) (p: ResizeArray<int>) =
    let rec loop (r': ResizeArray<int>) (p': ResizeArray<int>) =
        if p'.Count = 0 then
            if r'.Count > maxCliqueNodes.Count then
                maxCliqueNodes.Clear()
                maxCliqueNodes.AddRange(r')
        else if r'.Count + p'.Count > maxCliqueNodes.Count then
            let copyP = ResizeArray(p')
            for i in 0 .. copyP.Count - 1 do
                let v = copyP.[i]
                let newR = ResizeArray(r')
                newR.Add(v)
                let newP = ResizeArray()
                for j in i + 1 .. copyP.Count - 1 do
                    let u = copyP.[j]
                    if adj.[v, u] then newP.Add(u)
                loop newR newP
    loop r p

[<EntryPoint>]
let main _ =
    try
        File.ReadAllLines FILENAME
        |> Array.iter (fun line ->
            let parts = line.Split('-')
            if parts.Length = 2 then
                let a = getNodeId parts.[0]
                let b = getNodeId parts.[1]
                adj.[a, b] <- true
                adj.[b, a] <- true)
    with _ -> ()

    if nodeCount > 0 then
        let p = ResizeArray(Seq.init nodeCount id)
        let r = ResizeArray()
        findMaxCliqueRecursive r p

        let result =
            maxCliqueNodes
            |> Seq.map (fun i -> nodeNames.[i])
            |> Seq.sort
            |> String.concat ","
        printfn "%s" result
    0
