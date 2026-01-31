
open System
open System.IO

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"

    let rules, updates =
        let split = Array.findIndex System.String.IsNullOrWhiteSpace lines
        let parseRule (s: string) =
            let a = s.Split('|')
            int a.[0], int a.[1]
        let parseUpdate (s: string) =
            s.Split(',') |> Array.map int |> Array.toList
        lines.[0..split-1] |> Array.map parseRule |> Array.toList,
        lines.[split+1..] |> Array.map parseUpdate |> Array.toList

    let isCorrect update =
        let pos = update |> List.mapi (fun i v -> v, i) |> Map.ofList
        rules |> List.forall (fun (x, y) ->
            match Map.tryFind x pos, Map.tryFind y pos with
            | Some px, Some py -> px <= py
            | _ -> true)

    let middle = function
        | [] -> 0
        | xs -> xs.[xs.Length / 2]

    updates
    |> List.filter isCorrect
    |> List.sumBy middle
    |> printfn "%d"
    0
