
open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let re = Regex(@"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>")
    let stars =
        File.ReadAllLines "input.txt"
        |> Array.choose (fun line ->
            let m = re.Match line
            if m.Success then
                Some (int m.Groups.[1].Value,
                      int m.Groups.[2].Value,
                      int m.Groups.[3].Value,
                      int m.Groups.[4].Value)
            else None)

    let mutable bestT = 0
    let mutable bestArea = Int32.MaxValue

    for t in 1..100000 do
        let mutable minX, maxX = Int32.MaxValue, Int32.MinValue
        let mutable minY, maxY = Int32.MaxValue, Int32.MinValue

        for x, y, vx, vy in stars do
            let nx = x + vx * t
            let ny = y + vy * t
            minX <- min minX nx
            maxX <- max maxX nx
            minY <- min minY ny
            maxY <- max maxY ny

        let area = (maxX - minX) + (maxY - minY)
        if area < bestArea then
            bestArea <- area
            bestT <- t

    printfn "%d" bestT
    0
