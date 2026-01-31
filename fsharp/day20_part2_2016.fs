
open System
open System.IO

[<EntryPoint>]
let main _ =
    let ranges =
        File.ReadAllLines("input.txt")
        |> Array.choose (fun line ->
            match line.Split '-' with
            | [| s; e |] -> Some (UInt32.Parse s, UInt32.Parse e)
            | _ -> None)

    let sorted = Array.sortBy (fun (s, _) -> s) ranges

    let merged =
        sorted
        |> Array.fold (fun acc (s, e) ->
            match acc with
            | [] -> [(s, e)]
            | (ps, pe) :: rest when pe >= s - 1u -> (ps, max pe e) :: rest
            | _ -> (s, e) :: acc) []
        |> List.rev
        |> List.toArray

    let merged =
        if merged.Length > 0 && merged.[merged.Length - 1] |> snd <> UInt32.MaxValue then
            Array.append merged [| (UInt32.MaxValue, 0u) |]
        else merged

    let totalAllowed =
        merged
        |> Array.pairwise
        |> Array.sumBy (fun ((_, pe), (s, _)) -> s - pe - 1u)

    printfn "%u" totalAllowed
    0
