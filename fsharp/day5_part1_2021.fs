
open System
open System.IO

let SIZE = 1000
let grid = Array2D.init SIZE SIZE (fun _ _ -> 0)

let updateGrid x1 y1 x2 y2 =
    if x1 = x2 then
        let yMin, yMax = min y1 y2, max y1 y2
        for y = yMin to yMax do
            grid.[x1, y] <- grid.[x1, y] + 1
    elif y1 = y2 then
        let xMin, xMax = min x1 x2, max x1 x2
        for x = xMin to xMax do
            grid.[x, y1] <- grid.[x, y1] + 1

let countOverlaps () =
    seq { for i = 0 to SIZE - 1 do
          for j = 0 to SIZE - 1 do
              if grid.[i, j] > 1 then yield () }
    |> Seq.length

let main () =
    File.ReadAllLines("input.txt")
    |> Array.iter (fun line ->
        let parts = line.Split([|','; '-'; '>'; ' '|], StringSplitOptions.RemoveEmptyEntries)
        let x1, y1, x2, y2 = int parts.[0], int parts.[1], int parts.[2], int parts.[3]
        updateGrid x1 y1 x2 y2)
    printfn "%d" (countOverlaps ())

main ()
