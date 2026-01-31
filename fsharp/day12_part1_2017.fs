
open System
open System.IO
open System.Collections.Generic

module private Dict =
    let getOrAdd (d:Dictionary<_,_>) key factory =
        match d.TryGetValue(key) with
        | true, v -> v
        | _ ->
            let v = factory()
            d.[key] <- v
            v

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let adj = Dictionary<int, List<int>>()

    for line in lines do
        // "X <-> a, b, c"
        let parts = line.Split([|" <-> "|], StringSplitOptions.None)
        let prog = int parts.[0]
        let conns =
            parts.[1].Split([|", "|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int

        let listProg = Dict.getOrAdd adj prog (fun () -> List())
        for c in conns do
            listProg.Add(c)
            let listC = Dict.getOrAdd adj c (fun () -> List())
            listC.Add(prog)

    // DFS from program 0
    let visited = HashSet<int>()
    let stack = Stack<int>()
    stack.Push(0)
    while stack.Count > 0 do
        let cur = stack.Pop()
        if visited.Add(cur) then
            match adj.TryGetValue(cur) with
            | true, neigh -> for n in neigh do if not (visited.Contains n) then stack.Push n
            | _ -> ()

    printfn "%d" visited.Count
    0
