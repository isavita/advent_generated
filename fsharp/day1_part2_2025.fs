
open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let text = File.ReadAllText "input.txt"
    let pattern = Regex(@"([RL])\s*(-?\d+)", RegexOptions.Compiled)
    let matches = pattern.Matches(text)
    let mutable pos = 50
    let mutable hits = 0

    let floorDiv a b =
        let q = a / b
        if a < 0 && a % b <> 0 then q - 1 else q

    for m in matches do
        let dir = m.Groups.[1].Value.[0]
        let amt = int m.Groups.[2].Value
        if dir = 'R' then
            let newPos = pos + amt
            hits <- hits + newPos / 100
            pos <- newPos % 100
        else
            hits <- hits + floorDiv (pos - 1) 100 - floorDiv (pos - amt - 1) 100
            pos <- (pos - amt) % 100
            if pos < 0 then pos <- pos + 100

    printfn "%d" hits
    0
