
open System
open System.IO
open System.Text.RegularExpressions

let parseLine (line:string) =
    let m = Regex.Match(line, @"^([a-z-]+)(\d+)\[([a-z]{5})\]")
    if m.Success then
        let name = m.Groups.[1].Value.Replace("-", "")
        let sector = int m.Groups.[2].Value
        let checksum = m.Groups.[3].Value
        Some (name, sector, checksum)
    else None

let isReal (name:string) (checksum:string) =
    let counts = Array.zeroCreate<int> 26
    for c in name do
        counts.[int c - int 'a'] <- counts.[int c - int 'a'] + 1
    let letters =
        counts
        |> Array.mapi (fun i c -> (i, c))
        |> Array.sortBy (fun (i, c) -> (-c, i))
        |> Array.take 5
        |> Array.map (fun (i, _) -> char (i + int 'a'))
        |> String
    letters = checksum

[<EntryPoint>]
let main _ =
    let total =
        File.ReadLines("input.txt")
        |> Seq.choose parseLine
        |> Seq.filter (fun (name, _, checksum) -> isReal name checksum)
        |> Seq.sumBy (fun (_, sector, _) -> sector)
    printfn "%d" total
    0
