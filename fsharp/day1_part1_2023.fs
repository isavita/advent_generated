
open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let sum =
        File.ReadAllLines "input.txt"
        |> Array.sumBy (fun line ->
            let m = Regex.Matches(line, "\d")
            if m.Count > 0 then
                let a = int (string m.[0].Value)
                let b = int (string m.[m.Count-1].Value)
                a * 10 + b
            else 0)
    printfn "%d" sum
    0
