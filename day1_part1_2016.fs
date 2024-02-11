module Day1

let input = System.IO.File.ReadAllText("input.txt")

let directions = input.Split([|','; ' '|], System.StringSplitOptions.RemoveEmptyEntries)

let mutable x = 0
let mutable y = 0
let mutable dir = 0

for direction in directions do
    let turn = direction.[0]
    let steps = int direction.[1..]

    if turn = 'R' then
        dir <- (dir + 1) % 4
    else
        dir <- (dir + 3) % 4

    match dir with
    | 0 -> y <- y + steps
    | 1 -> x <- x + steps
    | 2 -> y <- y - steps
    | 3 -> x <- x - steps

let result = abs x + abs y
printfn "%d" result