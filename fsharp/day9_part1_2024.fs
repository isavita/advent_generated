
open System
open System.IO

[<EntryPoint>]
let main _ =
    let input = File.ReadAllText "input.txt"
    if String.IsNullOrEmpty input then
        printfn "0"
        0
    else
        let blocks =
            input.TrimEnd('\n')
            |> Seq.mapi (fun i c -> (i, int c - int '0'))
            |> Seq.filter (fun (_, len) -> len > 0)
            |> Seq.toArray

        let totalSize = blocks |> Array.sumBy snd
        let disk = Array.zeroCreate totalSize

        let mutable pos = 0
        for i, len in blocks do
            let id = if i % 2 = 0 then Some (i / 2) else None
            for _ in 1..len do
                disk.[pos] <- id
                pos <- pos + 1

        let mutable left = 0
        let mutable right = totalSize - 1
        while left < right do
            while left < right && disk.[left].IsSome do left <- left + 1
            while left < right && disk.[right].IsNone do right <- right - 1
            if left < right then
                disk.[left] <- disk.[right]
                disk.[right] <- None
                left  <- left  + 1
                right <- right - 1

        let checksum =
            disk
            |> Array.mapi (fun i fid -> match fid with Some f -> uint64 i * uint64 f | None -> 0UL)
            |> Array.sum
        printfn "%d" checksum
        0
