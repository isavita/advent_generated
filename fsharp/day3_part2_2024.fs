
open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let txt = File.ReadAllText "input.txt"
    let mul = Regex.Matches(txt, "mul\((\d+),(\d+)\)|do\(\)|don't\(\)")
    let mutable enabled = true
    let mutable total = 0
    for m in mul do
        match m.Value with
        | "do()" -> enabled <- true
        | "don't()" -> enabled <- false
        | _ when enabled ->
            let a = int (m.Groups.[1].Value)
            let b = int (m.Groups.[2].Value)
            total <- total + a * b
        | _ -> ()
    printfn "%d" total
    0
