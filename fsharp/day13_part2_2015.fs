
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    // Read and parse input
    let lines = File.ReadAllLines "input.txt"
    let mutable map : Map<string, Map<string,int>> = Map.empty
    for line in lines do
        let parts = line.Split ' '
        if parts.Length >= 11 then
            let from = parts.[0]
            let toGuest = parts.[10].TrimEnd('.')
            let value = int parts.[3]
            let change = if parts.[2] = "lose" then -value else value
            let inner = map |> Map.tryFind from |> Option.defaultValue Map.empty
            let inner' = inner |> Map.add toGuest change
            map <- map |> Map.add from inner'

    // Add "You" with zero contributions
    let mapWithYou =
        map |> Map.map (fun _ inner -> inner |> Map.add "You" 0)
    let mapYou =
        mapWithYou |> Map.toSeq |> Seq.map (fun (k,_) -> k,0) |> Map.ofSeq
    let map = mapWithYou |> Map.add "You" mapYou

    // List of guests
    let guests = map |> Map.toList |> List.map fst

    // Generate all permutations
    let rec permutations lst =
        seq {
            match lst with
            | [] -> yield []
            | _ ->
                for i in 0 .. List.length lst - 1 do
                    let head = List.item i lst
                    let tail = List.filter (fun x -> x <> head) lst
                    for perm in permutations tail do
                        yield head :: perm
        }

    // Compute happiness for an arrangement
    let happiness arrangement =
        let n = List.length arrangement
        arrangement
        |> List.mapi (fun i person ->
            let left = arrangement.[(i + n - 1) % n]
            let right = arrangement.[(i + 1) % n]
            let inner = map.[person]
            inner.[left] + inner.[right]
        )
        |> List.sum

    // Find maximum happiness
    let maxHappiness =
        permutations guests
        |> Seq.map happiness
        |> Seq.max

    printfn "%d" maxHappiness
    0
