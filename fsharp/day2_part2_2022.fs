
open System.IO

[<EntryPoint>]
let main _ =
    File.ReadAllLines "input.txt"
    |> Array.sumBy (fun line ->
        let opp, res = line.[0], line.[2]
        let shape =
            match res with
            | 'X' -> (int opp - int 'A' + 2) % 3   // lose
            | 'Y' -> int opp - int 'A'              // draw
            | _   -> (int opp - int 'A' + 1) % 3  // win
        let score = shape + 1
        score + (if res = 'Y' then 3 elif res = 'Z' then 6 else 0))
    |> printfn "%d"
    0
