
module Day6

open System.IO

let lines = File.ReadAllLines "input.txt"

type Point = { X: int; Y: int }

let mutable maxX = 0
let mutable maxY = 0
let points = ResizeArray<Point>()

for line in lines do
    let coords = line.Split([|','|])
    let x = int coords.[0]
    let y = int coords.[1]
    if x > maxX then maxX <- x
    if y > maxY then maxY <- y
    points.Add({ X = x; Y = y })

let grid = Array2D.create (maxX + 2) (maxY + 2) 0
let mutable areas = Array.zeroCreate (points.Count)
let mutable infinite = Array.zeroCreate (points.Count)

for i in 0 .. maxX + 1 do
    for j in 0 .. maxY + 1 do
        let mutable minDist = maxX + maxY
        for k = 0 to points.Count - 1 do
            let point = points.[k]
            let dist = abs (point.X - i) + abs (point.Y - j)
            if dist < minDist then
                minDist <- dist
                grid.[i, j] <- k
            elif dist = minDist then
                grid.[i, j] <- -1
        if grid.[i, j] <> -1 then
            if i = 0 || j = 0 || i = maxX + 1 || j = maxY + 1 then
                infinite.[grid.[i, j]] <- true
            areas.[grid.[i, j]] <- areas.[grid.[i, j]] + 1

let mutable maxArea = 0

for i = 0 to areas.Length - 1 do
    let area = areas.[i]
    if not infinite.[i] && area > maxArea then
        maxArea <- area

printfn "%d" maxArea

let abs x = if x < 0 then -x else x
