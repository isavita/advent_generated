
open System.IO

let parseMirror (block:string[]) =
    let rows = Array.zeroCreate block.Length
    let cols = Array.zeroCreate block.[0].Length
    for y in 0..block.Length-1 do
        let mutable r = 0
        for x in 0..block.[y].Length-1 do
            r <- (r <<< 1) ||| if block.[y].[x] = '#' then 1 else 0
        rows.[y] <- r
    for x in 0..block.[0].Length-1 do
        let mutable c = 0
        for y in 0..block.Length-1 do
            c <- (c <<< 1) ||| if block.[y].[x] = '#' then 1 else 0
        cols.[x] <- c
    rows, cols

let getMirrorAxisWithOneSmudge (lines:int[]) =
    let len = lines.Length
    [1..len-1] |> List.tryPick (fun i ->
        let minLen = min i (len - i)
        let mutable smudges = 0
        let ok = ref true
        for j in 0..minLen-1 do
            let a = lines.[i-1-j]
            let b = lines.[i+j]
            if a <> b then
                let diff = a ^^^ b
                if smudges > 0 || (diff &&& (diff - 1)) <> 0 then ok := false
                else smudges <- smudges + 1
        if !ok && smudges = 1 then Some i else None
    ) |> Option.defaultValue 0

[<EntryPoint>]
let main _ =
    let blocks = File.ReadAllText("input.txt").Split([|"\r\n\r\n"; "\n\n"|], System.StringSplitOptions.RemoveEmptyEntries)
    let mutable res = 0
    for block in blocks do
        let rows, cols = parseMirror (block.Split([|'\r'; '\n'|], System.StringSplitOptions.RemoveEmptyEntries))
        let c = getMirrorAxisWithOneSmudge cols
        let r = getMirrorAxisWithOneSmudge rows
        res <- res + c + r * 100
    printfn "%d" res
    0
