
open System
open System.IO
open System.Linq

let firstNUnique (s: string) n =
    Seq.windowed n s
    |> Seq.findIndex (fun window -> window |> Seq.distinct |> Seq.length = n)
    |> (+) n

[<EntryPoint>]
let main argv =
    let s = File.ReadAllText("input.txt").Trim()
    printfn "%d" (firstNUnique s 14)
    0
