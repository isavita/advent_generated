
open System
open System.Collections.Generic
open System.IO

let readAndParseInput (filename: string) =
    File.ReadLines(filename)
    |> Seq.fold (fun (distances: Map<string, Map<string, int>>) line ->
        let parts = line.Split(' ')
        if parts.Length = 5 then
            let from = parts.[0]
            let toLoc = parts.[2]
            let dist = int parts.[4]
            let distances =
                distances
                |> Map.change from (function None -> Some(Map.empty |> Map.add toLoc dist) | Some m -> Some(m |> Map.add toLoc dist))
                |> Map.change toLoc (function None -> Some(Map.empty |> Map.add from dist) | Some m -> Some(m |> Map.add from dist))
            distances
        else distances) Map.empty

let getUniqueLocations (distances: Map<string, Map<string, int>>) =
    distances
    |> Map.fold (fun locations from toDist ->
        Seq.append (Seq.singleton from) (Map.toSeq toDist |> Seq.map fst)
        |> Seq.fold (fun locations loc -> Set.add loc locations) locations) Set.empty
    |> Set.toList

let rec permute (arr: 'a list) =
    match arr with
    | [_] -> Seq.singleton arr
    | _ ->
        seq {
            for i = 0 to List.length arr - 1 do
                let x = List.item i arr
                let xs = List.append (List.take i arr) (List.skip (i + 1) arr)
                for p in permute xs do
                    yield x :: p
        }

let calculateRouteDistance (route: string list) (distances: Map<string, Map<string, int>>) =
    List.pairwise route
    |> List.sumBy (fun (from, toLoc) -> distances.[from].[toLoc])

let findShortestRoute (locations: string list) (distances: Map<string, Map<string, int>>) =
    permute locations
    |> Seq.map (fun route -> calculateRouteDistance route distances)
    |> Seq.min

let main () =
    let distances = readAndParseInput "input.txt"
    let locations = getUniqueLocations distances
    let minDistance = findShortestRoute locations distances
    printfn "%d" minDistance

main ()
