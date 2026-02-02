
open System
open System.IO

type Point = { x: int; y: int }

let readPointsAndFolds (filePath: string) =
    let lines = File.ReadAllLines(filePath)
    let mutable readingPoints = true
    let mutable points = []
    let mutable folds = []
    for line in lines do
        if line = "" then
            readingPoints <- false
        else
            if readingPoints then
                let parts = line.Split(',')
                let x = int parts.[0]
                let y = int parts.[1]
                points <- { x = x; y = y } :: points
            else
                let parts = line.Split('=')
                let axis = parts.[0].Substring(parts.[0].Length - 1)
                let value = int parts.[1]
                if axis = "x" then
                    folds <- { x = value; y = 0 } :: folds
                else
                    folds <- { x = 0; y = value } :: folds
    (List.rev points, List.rev folds)

let foldPoints (points: Point list) (fold: Point) =
    points
    |> List.map (fun p ->
        if fold.x <> 0 && p.x > fold.x then
            { x = fold.x - (p.x - fold.x); y = p.y }
        else if fold.y <> 0 && p.y > fold.y then
            { x = p.x; y = fold.y - (p.y - fold.y) }
        else
            p)
    |> List.distinct

let printGrid (points: Point list) =
    let maxX = points |> List.map (fun p -> p.x) |> List.max
    let maxY = points |> List.map (fun p -> p.y) |> List.max
    let grid = Array2D.init (maxY + 1) (maxX + 1) (fun _ _ -> ' ')
    points |> List.iter (fun p -> grid.[p.y, p.x] <- '#')
    for y in 0 .. maxY do
        for x in 0 .. maxX do
            printf "%c" grid.[y, x]
        printfn ""

let main() =
    let (points, folds) = readPointsAndFolds "input.txt"
    let mutable currentPoints = points
    for i = 0 to folds.Length - 1 do
        currentPoints <- foldPoints currentPoints folds.[i]
        if i = 0 then
            printfn "Number of dots visible after first fold: %d" (List.length currentPoints)
    printfn "Final grid:"
    printGrid currentPoints

main()
