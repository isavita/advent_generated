
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Monkey = { Items: Queue<int64>; Op: int64 -> int64; Div: int64; T: int; F: int; mutable C: int64 }

[<EntryPoint>]
let main _ =
    let input = File.ReadAllText("input.txt").Split([|"\r\n\r\n"; "\n\n"|], StringSplitOptions.RemoveEmptyEntries)
    let ms = input |> Array.map (fun b ->
        let l = b.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.Trim())
        let itms = Regex.Matches(l.[1], "\d+") |> Seq.cast<Match> |> Seq.map (fun m -> int64 m.Value)
        let opM = Regex.Match(l.[2], @"new = old ([\+\*]) (\w+)")
        let opS, opV = opM.Groups.[1].Value, opM.Groups.[2].Value
        let op = 
            match opS, opV with
            | "*", "old" -> fun x -> x * x
            | "*", v -> let v' = int64 v in fun x -> x * v'
            | "+", v -> let v' = int64 v in fun x -> x + v'
            | _ -> id
        { Items = Queue<int64>(itms); Op = op; Div = int64 (Regex.Match(l.[3], "\d+").Value); 
          T = int (Regex.Match(l.[4], "\d+").Value); F = int (Regex.Match(l.[5], "\d+").Value); C = 0L }
    )
    let mb = ms |> Array.fold (fun a m -> a * m.Div) 1L
    for _ in 1..10000 do
        for m in ms do
            while m.Items.Count > 0 do
                m.C <- m.C + 1L
                let i = (m.Op (m.Items.Dequeue())) % mb
                let target = if i % m.Div = 0L then m.T else m.F
                ms.[target].Items.Enqueue(i)
    let r = ms |> Array.map (fun m -> m.C) |> Array.sortDescending
    printfn "%d" (r.[0] * r.[1])
    0

