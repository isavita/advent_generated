
open System
open System.IO
open System.Collections.Generic

type Tile = { Id:int; Data:char[][]; Borders:string[] }

let rev (s:string) = new string(Array.rev (s.ToCharArray()))

let calculateBorders (data:char[][]) =
    let n = data.Length
    let top = String(data.[0])
    let bottom = String(data.[n-1])
    let left = String([| for i in 0..n-1 -> data.[i].[0] |])
    let right = String([| for i in 0..n-1 -> data.[i].[n-1] |])
    [| top; right; bottom; left; rev top; rev right; rev bottom; rev left |]

let readTiles (file:string) =
    let lines = File.ReadAllLines(file)
    let tiles = ResizeArray<Tile>()
    let mutable id = 0
    let data = ResizeArray<string>()
    for line in lines do
        if line.StartsWith("Tile") then
            id <- int (line.Substring(5, line.Length-6).Trim())
        elif line = "" then
            if data.Count > 0 then
                let arr = data |> Seq.map (fun s -> s.ToCharArray()) |> Seq.toArray
                tiles.Add({ Id=id; Data=arr; Borders=calculateBorders arr })
                data.Clear()
        else
            data.Add(line)
    if data.Count > 0 then
        let arr = data |> Seq.map (fun s -> s.ToCharArray()) |> Seq.toArray
        tiles.Add({ Id=id; Data=arr; Borders=calculateBorders arr })
    tiles |> Seq.toList

let buildBorderMap (tiles:Tile list) =
    let dict = Dictionary<string,int>()
    for t in tiles do
        for b in t.Borders do
            match dict.TryGetValue(b) with
            | true, v -> dict.[b] <- v + 1
            | _ -> dict.[b] <- 1
    dict

let findCornerProduct (tiles:Tile list) (borderMap:Dictionary<string,int>) =
    tiles
    |> List.fold (fun acc t ->
        let unique = t.Borders.[0..3] |> Array.filter (fun b -> borderMap.[b] = 1) |> Array.length
        if unique = 2 then acc * int64 t.Id else acc) 1L

[<EntryPoint>]
let main _ =
    let tiles = readTiles "input.txt"
    let borderMap = buildBorderMap tiles
    let product = findCornerProduct tiles borderMap
    printfn "%d" product
    0
