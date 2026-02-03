
open System
open System.IO
open System.Collections.Generic
open System.Linq

[<EntryPoint>]
let main argv =
    let root = [""]
    let dirs = Dictionary<string,int>()
    let files = Dictionary<string,int>()
    let mutable curr = List<string>(root)

    for line in File.ReadLines "input.txt" do
        let txt = line.Split(' ')
        if txt.[0] = "$" then
            if txt.[1] = "cd" then
                match txt.[2] with
                | "/" -> curr <- List<string>(root)
                | ".." -> curr.RemoveAt(curr.Count - 1)
                | dir -> curr.Add(dir)
                dirs.[String.Join("/", curr)] <- 0
        elif txt.[0] <> "dir" then
            let size = int txt.[0]
            let path = String.Join("/", seq { yield! curr; yield txt.[1] })
            files.[path] <- size

    for kv in files do
        let parts = kv.Key.Split('/')
        for i = 1 to parts.Length - 1 do
            let key = String.Join("/", parts.[0..i-1])
            dirs.[key] <- dirs.[key] + kv.Value

    let sorted = dirs.Values |> Seq.toArray |> Array.sort
    let total = 70000000
    let want = 30000000
    let available = total - dirs.[""]
    let needed = want - available

    if sorted.Length > 0 then
        let idx = Array.BinarySearch(sorted, needed)
        let result =
            if idx >= 0 then sorted.[idx]
            else
                let ins = ~~~idx
                if ins < sorted.Length then sorted.[ins] else -1
        if result <> -1 then printfn "%d" result else printfn "Desired size not found."
    else
        printfn "No sizes available."
    0
