
open System
open System.IO
open System.Text.RegularExpressions

type Star = { x:int; y:int; vx:int; vy:int }

let stars =
    let rgx = Regex @"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>"
    File.ReadAllLines "input.txt"
    |> Array.choose (fun line ->
        let m = rgx.Match line
        if m.Success then
            Some { x = int m.Groups.[1].Value
                   y = int m.Groups.[2].Value
                   vx = int m.Groups.[3].Value
                   vy = int m.Groups.[4].Value }
        else None)
    |> Array.toList

let bestT =
    let mutable bestT = 0
    let mutable bestArea = System.Int32.MaxValue
    for t in 1..100000 do
        let mutable minX = System.Int32.MaxValue
        let mutable minY = System.Int32.MaxValue
        let mutable maxX = System.Int32.MinValue
        let mutable maxY = System.Int32.MinValue
        for s in stars do
            let x = s.x + s.vx * t
            let y = s.y + s.vy * t
            if x < minX then minX <- x
            if x > maxX then maxX <- x
            if y < minY then minY <- y
            if y > maxY then maxY <- y
        let area = (maxX - minX + 1) + (maxY - minY + 1)
        if area < bestArea then
            bestArea <- area
            bestT <- t
    bestT

let finalStars =
    stars |> List.map (fun s ->
        { s with x = s.x + s.vx * bestT
                 y = s.y + s.vy * bestT })

let minX = finalStars |> List.map (fun s -> s.x) |> List.min
let minY = finalStars |> List.map (fun s -> s.y) |> List.min
let maxX = finalStars |> List.map (fun s -> s.x) |> List.max
let maxY = finalStars |> List.map (fun s -> s.y) |> List.max

let width = maxX - minX + 1
let height = maxY - minY + 1
let grid = Array2D.create height width ' '
for s in finalStars do
    grid.[s.y - minY, s.x - minX] <- '#'

for y in 0..height-1 do
    let line = Array.zeroCreate width
    for x in 0..width-1 do
        line.[x] <- grid.[y,x]
    printfn "%s" (System.String line)
