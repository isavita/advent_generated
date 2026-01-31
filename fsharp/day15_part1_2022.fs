
open System
open System.IO
open System.Text.RegularExpressions

[<Struct>]
type Point = { X:int; Y:int }

[<Struct>]
type Sensor = { Pos:Point; Beacon:Point; Dist:int }

let manhattan p q = abs(p.X-q.X) + abs(p.Y-q.Y)

let impossible sensors y =
    let mutable minX,maxX = Int32.MaxValue,Int32.MinValue
    for s in sensors do
        let d = s.Dist - abs(s.Pos.Y - y)
        if d >= 0 then
            let lo = s.Pos.X - d
            let hi = s.Pos.X + d
            if lo < minX then minX <- lo
            if hi > maxX then maxX <- hi
    if minX = Int32.MaxValue then 0 else
    let range = maxX-minX+1
    let covered = Array.zeroCreate<byte> range
    for s in sensors do
        let d = s.Dist - abs(s.Pos.Y - y)
        if d >= 0 then
            let lo = max minX (s.Pos.X - d)
            let hi = min maxX (s.Pos.X + d)
            for x in lo..hi do covered.[x-minX] <- 1uy
    for s in sensors do
        if s.Beacon.Y = y && s.Beacon.X >= minX && s.Beacon.X <= maxX then
            covered.[s.Beacon.X-minX] <- 0uy
    covered |> Array.sumBy int

[<EntryPoint>]
let main _ =
    let rx = Regex(@"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")
    let sensors =
        File.ReadLines "input.txt"
        |> Seq.choose (fun l -> let m = rx.Match l in if m.Success then Some m else None)
        |> Seq.map (fun m ->
            let pos = { X=int m.Groups.[1].Value; Y=int m.Groups.[2].Value }
            let beacon = { X=int m.Groups.[3].Value; Y=int m.Groups.[4].Value }
            { Pos=pos; Beacon=beacon; Dist=manhattan pos beacon })
        |> Seq.toArray
    printfn "%d" (impossible sensors 2000000)
    0
