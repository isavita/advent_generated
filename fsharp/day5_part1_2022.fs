open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let mutable i = 0
    let stackLines = ResizeArray<string>()
    while i < lines.Length && not (String.IsNullOrWhiteSpace(lines.[i])) do
        stackLines.Add(lines.[i])
        i <- i + 1
    let numStacks =
        stackLines.[stackLines.Count-1]
            .Trim()
            .Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.last
            |> int
    let stacks = Array.init numStacks (fun _ -> Stack<char>())
    for idx = stackLines.Count-2 downto 0 do
        let line = stackLines.[idx]
        let mutable j = 1
        while j < line.Length do
            let c = line.[j]
            if Char.IsLetter c then stacks.[(j-1)/4].Push(c)
            j <- j + 4
    for k = i+1 to lines.Length-1 do
        let line = lines.[k].Trim()
        if line.StartsWith("move") then
            let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let n = int parts.[1]
            let from = int parts.[3] - 1
            let to_ = int parts.[5] - 1
            for _ = 1 to n do
                stacks.[to_].Push(stacks.[from].Pop())
    let result = stacks |> Array.map (fun s -> s.Peek()) |> String.Concat
    printfn "%s" result
    0