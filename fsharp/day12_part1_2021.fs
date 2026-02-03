
open System
open System.IO

let isSmall (s:string) = Char.IsLower s.[0]

let addEdge (a:string) (b:string) (g:Map<string, Set<string>>) =
    g |> Map.change a (function
        | None -> Some (Set.singleton b)
        | Some s -> Some (Set.add b s))

let lines = File.ReadAllLines "input.txt"

let graph =
    lines
    |> Array.map (fun l -> let p = l.Split '-' in (p.[0], p.[1]))
    |> Array.fold (fun g (a,b) -> g |> addEdge a b |> addEdge b a) Map.empty

let rec dfs (node:string) (visited:Set<string>) : int64 =
    if node = "end" then 1L
    else
        let next = Map.find node graph
        next
        |> Seq.filter (fun n ->
            not (isSmall n && Set.contains n visited))
        |> Seq.sumBy (fun n ->
            let visited' = if isSmall n then Set.add n visited else visited
            dfs n visited')

[<EntryPoint>]
let main _ =
    let result = dfs "start" (Set.ofList ["start"])
    printfn "%d" result
    0
