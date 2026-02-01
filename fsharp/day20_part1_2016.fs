
open System
open System.IO

type IpRange = { Start:uint32; End:uint32 }

[<EntryPoint>]
let main _ =
    let ranges =
        File.ReadLines "input.txt"
        |> Seq.map (fun l ->
            let parts = l.Split '-'
            { Start = uint32 parts.[0]; End = uint32 parts.[1] })
        |> Seq.toList
        |> List.sortBy (fun r -> r.Start)

    let rec find ip = function
        | [] -> ip
        | h::t ->
            if h.Start > ip then ip
            elif h.End >= ip then find (h.End + 1u) t
            else find ip t

    let answer = find 0u ranges
    printfn "%u" answer
    0
