open System
open System.IO
open System.Collections.Generic

type Node(name:string) =
    member val Name = name with get,set
    member val Children : List<Node> = List<Node>() with get,set
    member val Parent : Node option = None with get,set

let findOrCreateNode (name:string) (nodes:Dictionary<string, Node>) =
    match nodes.TryGetValue(name) with
    | true, node -> node
    | _ ->
        let node = Node(name)
        nodes.Add(name, node)
        node

let buildOrbitMap (lines:string[]) =
    let nodes = Dictionary<string, Node>()
    for line in lines do
        let parts = line.Split(')')
        let center = findOrCreateNode parts.[0] nodes
        let orbiter = findOrCreateNode parts.[1] nodes
        center.Children.Add(orbiter)
        orbiter.Parent <- Some center
    nodes

let pathToRoot (node:Node) =
    let mutable n:Node option = Some node
    let path = ResizeArray<Node>()
    while n.IsSome do
        path.Add(n.Value)
        n <- n.Value.Parent
    path |> List.ofSeq

let findCommonAncestor (node1:Node) (node2:Node) =
    let path1 = pathToRoot node1
    let path2 = pathToRoot node2
    let mutable i = List.length path1 - 1
    let mutable j = List.length path2 - 1
    while i >= 0 && j >= 0 && path1.[i] = path2.[j] do
        i <- i - 1
        j <- j - 1
    (i + 1, j + 1)

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines "input.txt"
    let orbitMap = buildOrbitMap lines
    let youParent = orbitMap.["YOU"].Parent.Value
    let sanParent = orbitMap.["SAN"].Parent.Value
    let (dist1, dist2) = findCommonAncestor youParent sanParent
    printfn "%d" (dist1 + dist2)
    0