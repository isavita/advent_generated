
open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let size = 1000
    let fabric = Array2D.create size size 0
    let regex = Regex(@"#\d+ @ (\d+),(\d+): (\d+)x(\d+)")
    File.ReadLines("input.txt")
    |> Seq.iter (fun line ->
        let m = regex.Match(line)
        if m.Success then
            let left = int m.Groups.[1].Value
            let top = int m.Groups.[2].Value
            let w = int m.Groups.[3].Value
            let h = int m.Groups.[4].Value
            for i = top to top + h - 1 do
                for j = left to left + w - 1 do
                    fabric.[i,j] <- fabric.[i,j] + 1)
    let total =
        seq {
            for i = 0 to size-1 do
                for j = 0 to size-1 do
                    if fabric.[i,j] >= 2 then yield ()
        } |> Seq.length
    printfn "%d" total
    0
