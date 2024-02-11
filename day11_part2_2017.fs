module Day11

let input = System.IO.File.ReadAllText("input.txt")

let directions = input.Split(',')

let mutable x = 0
let mutable y = 0
let mutable z = 0
let mutable maxDistance = 0

for direction in directions do
    match direction with
    | "n" -> y <- y + 1; z <- z - 1
    | "ne" -> x <- x + 1; z <- z - 1
    | "se" -> x <- x + 1; y <- y - 1
    | "s" -> y <- y - 1; z <- z + 1
    | "sw" -> x <- x - 1; z <- z + 1
    | "nw" -> x <- x - 1; y <- y + 1
    | _ -> ()

    let distance = (abs x + abs y + abs z) / 2
    if distance > maxDistance then
        maxDistance <- distance

printfn "%d" (abs x + abs y + abs z)
printfn "%d" maxDistance