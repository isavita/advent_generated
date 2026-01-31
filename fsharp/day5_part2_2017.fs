
open System
open System.IO

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let n = lines.Length
    let offsets = Array.map int lines
    let mutable idx = 0
    let mutable steps = 0
    while idx >= 0 && idx < n do
        let jump = offsets.[idx]
        offsets.[idx] <- if jump >= 3 then jump - 1 else jump + 1
        idx <- idx + jump
        steps <- steps + 1
    printfn "%d" steps
    0
