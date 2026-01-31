
open System
open System.IO

let lines = File.ReadAllLines("input.txt")
let width = lines.[0].Length
let height = lines.Length

let mutable galaxies = []
for y in 0 .. height - 1 do
    for x in 0 .. width - 1 do
        if lines.[y].[x] = '#' then
            galaxies <- (x, y) :: galaxies
galaxies <- List.rev galaxies

let rowHasGalaxy = Array.init height (fun y -> List.exists (fun (_, gy) -> gy = y) galaxies)
let colHasGalaxy = Array.init width (fun x -> List.exists (fun (gx, _) -> gx = x) galaxies)

let rowOffset = Array.zeroCreate height
let mutable offset = 0
for y in 0 .. height - 1 do
    rowOffset.[y] <- offset
    if not rowHasGalaxy.[y] then offset <- offset + 1

let colOffset = Array.zeroCreate width
offset <- 0
for x in 0 .. width - 1 do
    colOffset.[x] <- offset
    if not colHasGalaxy.[x] then offset <- offset + 1

let expansionFactor = 1000000L
let expansionAdd = expansionFactor - 1L

let mutable total = 0L
let g = Array.ofList galaxies
for i in 0 .. g.Length - 1 do
    let (x1, y1) = g.[i]
    let x1' = int64 x1 + int64 colOffset.[x1] * expansionAdd
    let y1' = int64 y1 + int64 rowOffset.[y1] * expansionAdd
    for j in i + 1 .. g.Length - 1 do
        let (x2, y2) = g.[j]
        let x2' = int64 x2 + int64 colOffset.[x2] * expansionAdd
        let y2' = int64 y2 + int64 rowOffset.[y2] * expansionAdd
        total <- total + abs (x1' - x2') + abs (y1' - y2')

printfn "%d" total
