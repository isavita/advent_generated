
open System
open System.IO
open System.Text

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines("input.txt")
    if lines.Length = 0 then 0
    else
        let len = lines.[0].Length
        let counts = Array.init len (fun _ -> Array.create 128 0)
        for line in lines do
            for i = 0 to len - 1 do
                let c = int line.[i]
                counts.[i].[c] <- counts.[i].[c] + 1
        let sb = StringBuilder()
        for i = 0 to len - 1 do
            let col = counts.[i]
            let mutable maxIdx = 0
            for j = 1 to 127 do
                if col.[j] > col.[maxIdx] then maxIdx <- j
            sb.Append(char maxIdx) |> ignore
        printfn "%s" (sb.ToString())
        0
