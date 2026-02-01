open System
open System.IO
open System.Collections.Generic
open System.Linq

let rotate (s:string) =
    let parts = s.Split('/')
    let n = parts.Length
    let newParts = Array.zeroCreate<string> n
    for x in 0 .. n-1 do
        let sb = System.Text.StringBuilder()
        for y in n-1 .. -1 .. 0 do
            sb.Append(parts.[y].[x]) |> ignore
        newParts.[x] <- sb.ToString()
    String.Join("/", newParts)

let flip (s:string) =
    let parts = s.Split('/')
    let revParts = parts |> Array.map (fun p -> new string(p.ToCharArray() |> Array.rev))
    String.Join("/", revParts)

let memo = Dictionary<string,string>()

let enhance (input:string) (rules:Dictionary<string,string>) =
    match memo.TryGetValue(input) with
    | true, v -> v
    | _ ->
        let candidates seq = seq |> Seq.tryPick (fun s -> match rules.TryGetValue(s) with | true, out -> Some out | _ -> None)
        let first =
            candidates [input; rotate input; rotate (rotate input); rotate (rotate (rotate input))]
        let result =
            match first with
            | Some v -> v
            | None ->
                let flipInput = flip input
                let firstFlip =
                    candidates [flipInput; rotate flipInput; rotate (rotate flipInput); rotate (rotate (rotate flipInput))]
                match firstFlip with
                | Some v -> v
                | None -> ""
        memo.[input] <- result
        result

[<EntryPoint>]
let main _ =
    let rules = Dictionary<string,string>()
    use reader = new StreamReader("input.txt")
    let mutable line = reader.ReadLine()
    while not (isNull line) do
        let parts = line.Split(" => ")
        rules.[parts.[0]] <- parts.[1]
        line <- reader.ReadLine()

    let mutable grid = [|".#."; "..#"; "###"|]
    for _ in 1 .. 18 do
        let subSize = if grid.Length % 2 = 0 then 2 else 3
        let newSize = grid.Length / subSize * (subSize + 1)
        let newGrid = Array.init newSize (fun _ -> "")
        for y in 0 .. subSize .. grid.Length - 1 do
            for x in 0 .. subSize .. grid.Length - 1 do
                let square = Array.init subSize (fun dy -> grid.[y + dy].Substring(x, subSize))
                let newSquare = enhance (String.Join("/", square)) rules
                let newRows = newSquare.Split('/')
                for dy in 0 .. subSize do
                    let idx = y / subSize * (subSize + 1) + dy
                    newGrid.[idx] <- newGrid.[idx] + newRows.[dy]
        grid <- newGrid

    let count = grid |> Array.sumBy (fun row -> row |> Seq.filter ((=) '#') |> Seq.length)
    printfn "%d" count
    0
