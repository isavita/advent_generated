
open System.IO

let lines = File.ReadAllLines("input.txt")
let height = lines.Length
let width = lines.[0].Length
let grid = Array.init height (fun i -> lines.[i].ToCharArray())

let sx,sy =
    let mutable sx, sy = 0, 0
    for y in 0..height-1 do
        for x in 0..width-1 do
            if grid.[y].[x] = 'S' then
                sx <- x
                sy <- y
    sx, sy

let active = Array.zeroCreate width
let next = Array.zeroCreate width
let mutable splits = 0

active.[sx] <- 1

for y in sy..height-1 do
    Array.fill next 0 width 0
    for x in 0..width-1 do
        if active.[x] <> 0 then
            if grid.[y].[x] = '^' then
                splits <- splits + 1
                if x - 1 >= 0 then next.[x - 1] <- 1
                if x + 1 < width then next.[x + 1] <- 1
            else
                next.[x] <- 1
    Array.blit next 0 active 0 width
    if Array.forall (fun x -> x = 0) active then ()

printfn "%d" splits
