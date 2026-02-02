
open System.IO

[<EntryPoint>]
let main argv =
    File.ReadAllText("input.txt") |> printfn "%s"
    0
