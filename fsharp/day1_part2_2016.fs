module Day1

let input = System.IO.File.ReadAllText("input.txt")

let directions = input.Split([|','; ' '|], System.StringSplitOptions.RemoveEmptyEntries)

let mutable x = 0
let mutable y = 0
let mutable dir = 0
let mutable visited = Set.empty

let rec move (dx, dy) steps =
    for _ in 1..steps do
        x <- x + dx
        y <- y + dy
        if visited.Contains((x, y)) then
            printfn "%d" (abs x + abs y)
            System.Environment.Exit(0)
        visited <- visited.Add((x, y))

for direction in directions do
    let turn = if direction.[0] = 'R' then 1 else -1
    dir <- (dir + turn + 4) % 4
    let steps = int direction.[1..]
    match dir with
    | 0 -> move (0, 1) steps
    | 1 -> move (1, 0) steps
    | 2 -> move (0, -1) steps
    | 3 -> move (-1, 0) steps

printfn "Not found"