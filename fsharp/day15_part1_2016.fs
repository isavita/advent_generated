
open System
open System.IO

type Disc = { total:int; start:int }

let discs =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line ->
        let m = System.Text.RegularExpressions.Regex.Match(line,
                 @"Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+).")
        { total = int m.Groups.[1].Value
          start = int m.Groups.[2].Value })
    |> Array.toList

let rec gcd a b = if b = 0 then a else gcd b (a % b)
let lcm a b = a / (gcd a b) * b

[<EntryPoint>]
let main _ =
    let mutable time = 0
    let mutable step = 1
    discs |> List.iteri (fun i d ->
        let offset = (d.start + i + 1) % d.total          // (start + i + 1) mod total
        while (time + offset) % d.total <> 0 do
            time <- time + step
        step <- lcm step d.total)
    printfn "%d" time
    0
