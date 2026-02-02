
open System
open System.IO

let sign x = 
    match x with
    | x when x > 0 -> 1
    | x when x < 0 -> -1
    | _ -> 0

let abs x = 
    if x < 0 then -x else x

let main() =
    let grid = Array2D.init 1000 1000 (fun _ _ -> 0)
    File.ReadAllLines("input.txt")
    |> Seq.iter (fun line ->
        let parts = line.Split(" -> ")
        let (x1, y1) = parts.[0].Split(',') |> (fun x -> int x.[0], int x.[1])
        let (x2, y2) = parts.[1].Split(',') |> (fun x -> int x.[0], int x.[1])
        let xStep = sign (x2 - x1)
        let yStep = sign (y2 - y1)
        let steps = max (abs (x2-x1)) (abs (y2-y1))
        for i = 0 to steps do
            grid.[y1 + i*yStep, x1 + i*xStep] <- grid.[y1 + i*yStep, x1 + i*xStep] + 1
    )
    let count = 
        seq { for i in 0 .. 999 do
              for j in 0 .. 999 do
              if grid.[i, j] > 1 then yield () }
        |> Seq.length
    printfn "%d" count

main()
