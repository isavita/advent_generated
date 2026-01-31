
open System
open System.IO
open System.Text.RegularExpressions

type Claim = { ID:int; X:int; Y:int; W:int; H:int }

[<EntryPoint>]
let main argv =
    let pattern = @"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
    let claims =
        File.ReadAllLines("input.txt")
        |> Array.map (fun line ->
            let m = Regex.Match(line, pattern)
            { ID = int m.Groups.[1].Value
              X = int m.Groups.[2].Value
              Y = int m.Groups.[3].Value
              W = int m.Groups.[4].Value
              H = int m.Groups.[5].Value })
    let size = 1000
    let fabric = Array.init size (fun _ -> Array.zeroCreate<int> size)
    for c in claims do
        for y in c.Y .. c.Y + c.H - 1 do
            let row = fabric.[y]
            for x in c.X .. c.X + c.W - 1 do
                row.[x] <- row.[x] + 1
    let result =
        claims |> Array.find (fun c ->
            seq {
                for y in c.Y .. c.Y + c.H - 1 do
                    for x in c.X .. c.X + c.W - 1 do
                        yield fabric.[y].[x] > 1
            } |> Seq.exists id |> not)
    printfn "%d" result.ID
    0
