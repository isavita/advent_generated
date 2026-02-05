
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let preamble = 25
    let numbers = File.ReadAllLines("input.txt") |> Array.map Int64.Parse
    let mutable answer = 0L
    for i = preamble to numbers.Length - 1 do
        if answer = 0L then
            let target = numbers.[i]
            let seen = HashSet<Int64>()
            let mutable ok = false
            for j = i - preamble to i - 1 do
                if seen.Contains(target - numbers.[j]) then
                    ok <- true
                else
                    seen.Add(numbers.[j]) |> ignore
            if not ok then answer <- target
    printfn "%d" answer
    0
