
open System
open System.IO
open System.Text.RegularExpressions

type Nanobot = { X:int; Y:int; Z:int; Radius:int }

let parse line =
    let m = Regex.Match(line, @"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)")
    { X = int m.Groups.[1].Value
      Y = int m.Groups.[2].Value
      Z = int m.Groups.[3].Value
      Radius = int m.Groups.[4].Value }

let manhattan a b =
    abs (a.X - b.X) + abs (a.Y - b.Y) + abs (a.Z - b.Z)

[<EntryPoint>]
let main _ =
    let bots = File.ReadAllLines "input.txt" |> Array.map parse
    let strongest = Array.maxBy (fun b -> b.Radius) bots
    bots |> Array.filter (fun b -> manhattan b strongest <= strongest.Radius)
         |> Array.length
         |> printfn "%d"
    0
