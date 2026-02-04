open System
open System.IO
open System.Collections.Generic

type RangeMap = { SrcStart:int64; DestStart:int64; Length:int64 }

let convert (number:int64) (ranges:RangeMap list) =
    let rec loop = function
        | [] -> number
        | r::rs ->
            if number >= r.SrcStart && number < r.SrcStart + r.Length then
                r.DestStart + (number - r.SrcStart)
            else loop rs
    loop ranges

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines "input.txt" |> Array.map (fun s -> s.Trim()) |> Array.toList
    let seeds = ResizeArray<int64>()
    let maps = ResizeArray<ResizeArray<RangeMap>>()
    let mutable current = ResizeArray<RangeMap>()

    for line in lines do
        if line = "" then ()
        elif line.StartsWith("seeds:") then
            line.Substring(6).Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int64
            |> Array.iter seeds.Add
        elif line.EndsWith("map:") then
            if current.Count > 0 then
                maps.Add(current)
                current <- ResizeArray<RangeMap>()
        else
            let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let destStart = int64 parts.[0]
            let srcStart = int64 parts.[1]
            let length = int64 parts.[2]
            current.Add { SrcStart = srcStart; DestStart = destStart; Length = length }

    if current.Count > 0 then maps.Add(current)

    let minLoc =
        seeds
        |> Seq.map (fun seed ->
            maps
            |> Seq.fold (fun loc map -> convert loc (Seq.toList map)) seed)
        |> Seq.min

    printfn "%d" minLoc
    0