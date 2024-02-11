module Day3
let input = System.IO.File.ReadAllText("input.txt")

let mutable x, y = 0, 0
let mutable santaX, santaY = 0, 0
let mutable roboX, roboY = 0, 0
let mutable visited = Set.empty

let addVisited x y visited =
    Set.add (x, y) visited

for i in 0..input.Length-1 do
    if i % 2 = 0 then
        match input.[i] with
        | '^' -> y <- y + 1
        | 'v' -> y <- y - 1
        | '>' -> x <- x + 1
        | '<' -> x <- x - 1
    else
        match input.[i] with
        | '^' -> santaY <- santaY + 1
        | 'v' -> santaY <- santaY - 1
        | '>' -> santaX <- santaX + 1
        | '<' -> santaX <- santaX - 1
    visited <- addVisited x y visited
    visited <- addVisited santaX santaY visited

printfn "%i" (visited.Count)