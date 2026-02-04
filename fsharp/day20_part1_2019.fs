
open System
open System.IO
open System.Collections.Generic

type Pt = { X:int; Y:int }

let findPortals (m:char[][]) =
    let d = Dictionary<string, ResizeArray<Pt>>()
    let R = m.Length
    let C = m.[0].Length
    for r in 0 .. R-1 do
        for c in 0 .. C-1 do
            if Char.IsUpper m.[r].[c] then
                if r+1 < R && Char.IsUpper m.[r+1].[c] then
                    let name = string m.[r].[c] + string m.[r+1].[c]
                    let p = if r+2 < R && m.[r+2].[c] = '.' then {X=c;Y=r+2} else {X=c;Y=r-1}
                    if not (d.ContainsKey name) then d.[name] <- ResizeArray()
                    d.[name].Add p
                elif c+1 < C && Char.IsUpper m.[r].[c+1] then
                    let name = string m.[r].[c] + string m.[r].[c+1]
                    let p = if c+2 < C && m.[r].[c+2] = '.' then {X=c+2;Y=r} else {X=c-1;Y=r}
                    if not (d.ContainsKey name) then d.[name] <- ResizeArray()
                    d.[name].Add p
    d

let bfs (m:char[][]) (start:Pt) (goal:Pt) (portals:Dictionary<string, ResizeArray<Pt>>) =
    let R = m.Length
    let C = m.[0].Length
    let q = Queue<Pt * int>()
    let visited = HashSet<Pt>()
    q.Enqueue(start,0)
    visited.Add start |> ignore
    let dr = [| -1; 1; 0; 0 |]
    let dc = [| 0; 0; -1; 1 |]
    while q.Count > 0 do
        let (cur, steps) = q.Dequeue()
        if cur = goal then
            printfn "%d" steps
            Environment.Exit 0
        for i in 0 .. 3 do
            let nr = cur.Y + dr.[i]
            let nc = cur.X + dc.[i]
            if nr>=0 && nr<R && nc>=0 && nc<C && m.[nr].[nc] = '.' then
                let nxt = {X=nc; Y=nr}
                if visited.Add nxt then q.Enqueue(nxt, steps+1)
        for kv in portals do
            let name = kv.Key
            if name <> "AA" && name <> "ZZ" then
                let pts = kv.Value
                if pts.Contains cur then
                    let other = if pts.[0] = cur then pts.[1] else pts.[0]
                    if visited.Add other then q.Enqueue(other, steps+1)
    -1

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines "input.txt"
    let maze = lines |> Array.map (fun s -> s.ToCharArray())
    let portals = findPortals maze
    let start = portals.["AA"].[0]
    let goal = portals.["ZZ"].[0]
    let ans = bfs maze start goal portals
    printfn "%d" ans
    0
