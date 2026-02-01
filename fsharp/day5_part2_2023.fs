open System
open System.IO

type Range = {destStart:int64; srcStart:int64; length:int64}

let rec reverseConvert (n:int64) (ranges:Range list) =
    let rec loop rs =
        match rs with
        | [] -> n
        | r::rest ->
            if n >= r.destStart && n < r.destStart + r.length then
                r.srcStart + (n - r.destStart)
            else loop rest
    loop ranges

let inSeedRanges ranges n =
    ranges |> List.exists (fun (s,l) -> n >= s && n < s + l)

let main () =
    let lines = File.ReadAllLines "input.txt"
    let seedRanges =
        lines |> Array.find (fun l -> l.StartsWith "seeds:")
              |> fun l -> l.Substring(6).Trim()
              |> fun s -> s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
              |> Array.chunkBySize 2
              |> Array.map (fun arr -> (int64 arr[0], int64 arr[1]))
              |> Array.toList

    let rec parse idx acc current =
        if idx >= lines.Length then
            if List.isEmpty current then List.rev acc else List.rev (current :: acc)
        else
            let line = lines[idx]
            if line.Trim().EndsWith ":" then
                let newAcc = if List.isEmpty current then acc else (current :: acc)
                parse (idx+1) newAcc []
            elif String.IsNullOrWhiteSpace line then parse (idx+1) acc current
            else
                let parts = line.Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                if parts.Length = 3 then
                    let r = {destStart=int64 parts[0]; srcStart=int64 parts[1]; length=int64 parts[2]}
                    parse (idx+1) acc (r :: current)
                else parse (idx+1) acc current

    let maps = parse 0 [] []
    let mapsRev = List.rev maps

    let rec search loc =
        let seed = mapsRev |> List.fold (fun acc map -> reverseConvert acc map) loc
        if inSeedRanges seedRanges seed then
            printfn "%d" loc
        else
            search (loc + 1L)

    search 0L

[<EntryPoint>]
let entry argv =
    main()
    0