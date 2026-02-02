
open System
open System.Collections.Generic
open System.IO
open System.Linq

type Point = { X: int; Y: int }

let foldPoint (axis: int, value: int) point =
    match axis with
    | 0 when point.X > value -> { point with X = 2 * value - point.X }
    | 1 when point.Y > value -> { point with Y = 2 * value - point.Y }
    | _ -> point

let main () =
    try
        let lines = File.ReadAllLines("input.txt")
        let points, folds =
            lines
            |> Seq.takeWhile (fun l -> l <> "")
            |> Seq.map (fun l -> l.Split(',') |> Array.map int |> (fun arr -> { X = arr.[0]; Y = arr.[1] }))
            |> Seq.toList,
            lines
            |> Seq.skipWhile (fun l -> l = "" || not (l.StartsWith "fold along"))
            |> Seq.map (fun l ->
                let parts = l.Split('=')
                let axis = if parts.[0].EndsWith "x" then 0 else 1
                let value = int parts.[1]
                (axis, value))
            |> Seq.toList
        if folds.IsEmpty then
            printfn "0"
        else
            let axis, value = List.head folds
            let foldedPoints = points |> List.map (foldPoint (axis, value))
            let uniqueCount = foldedPoints |> Seq.distinct |> Seq.length
            printfn "%d" uniqueCount
    with
    | :? FileNotFoundException -> printfn "Error opening file"

main ()
