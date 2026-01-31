
open System
open System.IO
open System.Text.RegularExpressions

let getPosition (row:int64) (col:int64) =
    (row + col - 2L) * (row + col - 1L) / 2L + col

let modPow (base_:int64) (exp:int64) (modulus:int64) =
    let rec loop b e acc =
        if e = 0L then acc
        else
            let acc = if (e &&& 1L) = 1L then (acc * b) % modulus else acc
            let b = (b * b) % modulus
            loop b (e >>> 1) acc
    loop base_ exp 1L

[<EntryPoint>]
let main _ =
    let line = File.ReadAllText "input.txt"
    let m = Regex.Match(line, @"row (\d+), column (\d+)")
    let row = int64 m.Groups.[1].Value
    let col = int64 m.Groups.[2].Value
    let pos = getPosition row col
    let start = 20151125L
    let multiplier = 252533L
    let modulus = 33554393L
    let code = (start * modPow multiplier (pos - 1L) modulus) % modulus
    printfn "%d" code
    0
