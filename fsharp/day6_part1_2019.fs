
open System
open System.IO
open System.Collections.Generic

type Orbit = { Key: string; Value: string }

let countOrbits (orbits: Orbit array) (start: string) =
    let dict = new Dictionary<string, string list>()
    orbits |> Array.iter (fun o ->
        if dict.ContainsKey o.Key then
            dict.[o.Key] <- o.Value :: dict.[o.Key]
        else
            dict.[o.Key] <- [o.Value]
    )

    let rec count depth node =
        match dict.TryGetValue node with
        | true, children -> depth + (children |> List.sumBy (count (depth + 1)))
        | _ -> depth

    count 0 start

let main() =
    try
        let lines = File.ReadAllLines("input.txt")
        let orbits =
            lines
            |> Array.map (fun line ->
                let parts = line.Split(')')
                { Key = parts.[0]; Value = parts.[1].Trim() }
            )

        let totalOrbits = countOrbits orbits "COM"
        printfn "%d" totalOrbits
    with
    | :? FileNotFoundException -> printfn "Error opening file"

main()
