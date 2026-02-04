
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let input = File.ReadAllText("input.txt").Trim()
    let seq = input |> Seq.map (fun c -> int c - int '0') |> Seq.toArray
    let seqLen = seq.Length
    let scores = List<int>()
    scores.Add(3); scores.Add(7)
    let mutable elf1 = 0
    let mutable elf2 = 1
    let mutable found = false
    while not found do
        let sum = scores.[elf1] + scores.[elf2]
        if sum >= 10 then
            scores.Add(sum / 10)
            if scores.Count >= seqLen then
                let start = scores.Count - seqLen
                let mutable ok = true
                for i = 0 to seqLen - 1 do
                    if scores.[start + i] <> seq.[i] then ok <- false
                if ok then found <- true
        if not found then
            scores.Add(sum % 10)
            if scores.Count >= seqLen then
                let start = scores.Count - seqLen
                let mutable ok = true
                for i = 0 to seqLen - 1 do
                    if scores.[start + i] <> seq.[i] then ok <- false
                if ok then found <- true
        elf1 <- (elf1 + scores.[elf1] + 1) % scores.Count
        elf2 <- (elf2 + scores.[elf2] + 1) % scores.Count
    printfn "%d" (scores.Count - seqLen)
    0
