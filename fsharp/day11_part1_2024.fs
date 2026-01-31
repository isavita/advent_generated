
open System
open System.IO

let rec trimLeadingZeros (s:string) =
    if s.Length > 1 && s.[0] = '0' then trimLeadingZeros (s.Substring 1)
    else if s = "" then "0" else s

let step (stones:string list) =
    stones |> List.collect (fun s ->
        if s = "0" then ["1"]
        elif s.Length % 2 = 0 then
            let mid = s.Length / 2
            let left = trimLeadingZeros (s.Substring(0, mid))
            let right = trimLeadingZeros (s.Substring mid)
            [left; right]
        else
            [string (int64 s * 2024L)]
    )

[<EntryPoint>]
let main _ =
    let stones =
        File.ReadAllText("input.txt").Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
    let result = List.fold (fun acc _ -> step acc) stones [1..25]
    printfn "%d" result.Length
    0
