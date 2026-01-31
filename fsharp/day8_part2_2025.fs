
open System
open System.IO

[<Struct>]
type Point = { X:int; Y:int; Z:int }

[<Struct>]
type Edge = { U:int; V:int; D:int64 }

let inline distSq (a:Point) (b:Point) =
    let dx = int64 (a.X - b.X)
    let dy = int64 (a.Y - b.Y)
    let dz = int64 (a.Z - b.Z)
    dx*dx + dy*dy + dz*dz

let rec find (p:int[]) x =
    if p.[x] <> x then
        p.[x] <- p.[p.[x]]
        find p p.[x]
    else x

let unite (p:int[]) (r:int[]) a b =
    let ra = find p a
    let rb = find p b
    if ra <> rb then
        if r.[ra] < r.[rb] then p.[ra] <- rb
        elif r.[ra] > r.[rb] then p.[rb] <- ra
        else
            p.[rb] <- ra
            r.[ra] <- r.[ra] + 1

let lines = File.ReadAllLines "input.txt"
let pts =
    lines
    |> Array.choose (fun (s:string) ->
        let parts = s.Split(',')
        if parts.Length = 3 then
            Some { X = int parts.[0]; Y = int parts.[1]; Z = int parts.[2] }
        else None)

if pts.Length >= 2 then
    let n = pts.Length
    let edges =
        [| for i in 0..n-1 do
               for j in i+1..n-1 do
                   { U=i; V=j; D=distSq pts.[i] pts.[j] } |]
    Array.sortInPlaceBy (fun (e:Edge) -> e.D) edges
    let parent = Array.init n id
    let rank = Array.zeroCreate n
    let mutable comps = n
    let mutable found = false
    let mutable i = 0
    while not found && i < edges.Length do
        let e = edges.[i]
        let ru = find parent e.U
        let rv = find parent e.V
        if ru <> rv then
            unite parent rank ru rv
            comps <- comps - 1
            if comps = 1 then
                let p1 = pts.[e.U]
                let p2 = pts.[e.V]
                printfn "Connected %d,%d,%d and %d,%d,%d" p1.X p1.Y p1.Z p2.X p2.Y p2.Z
                printfn "Product of X coordinates: %d" (int64 p1.X * int64 p2.X)
                found <- true
        i <- i + 1
