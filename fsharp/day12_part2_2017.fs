
open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv =
    let adj = Dictionary<int, ResizeArray<int>>()
    let pattern = @"\d+"
    for line in File.ReadLines("input.txt") do
        let nums = Regex.Matches(line, pattern) |> Seq.map (fun m -> int m.Value) |> Seq.toArray
        if nums.Length > 0 then
            let u = nums.[0]
            if not (adj.ContainsKey(u)) then adj.[u] <- ResizeArray()
            for i = 1 to nums.Length - 1 do
                let v = nums.[i]
                if not (adj.ContainsKey(v)) then adj.[v] <- ResizeArray()
                adj.[u].Add(v)
                adj.[v].Add(u)
    let visited = HashSet<int>()
    let mutable groups = 0
    for kvp in adj do
        let node = kvp.Key
        if not (visited.Contains(node)) then
            groups <- groups + 1
            let stack = Stack<int>()
            stack.Push(node)
            visited.Add(node) |> ignore
            while stack.Count > 0 do
                let cur = stack.Pop()
                for nb in adj.[cur] do
                    if not (visited.Contains(nb)) then
                        visited.Add(nb) |> ignore
                        stack.Push(nb)
    printfn "%d" groups
    0
