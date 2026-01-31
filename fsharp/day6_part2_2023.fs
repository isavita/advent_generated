
open System
open System.IO
open System.Text.RegularExpressions

let ways (time:int64) (record:int64) =
    let b = -time
    let c = record + 1L
    let disc = b*b - 4L*c |> float |> sqrt |> int64
    let lo = (-b - disc) / 2L
    let hi = (-b + disc) / 2L
    hi - lo + if (lo*(time-lo)) = c then 1L else 0L

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let extract (s:string) =
        Regex.Replace(s, @"\D+", "") |> int64
    let time = extract lines.[0]
    let dist = extract lines.[1]
    printfn "%d" (ways time dist)
    0
