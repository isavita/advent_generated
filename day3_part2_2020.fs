module Day3

let input = System.IO.File.ReadAllLines("input.txt")

let rec countTrees (right:int) (down:int) (x:int) (y:int) (trees:int) =
    if y >= input.Length then trees
    else
        let width = input.[0].Length
        let newX = (x + right) % width
        let newY = y + down
        let newTrees = if input.[y].[x] = '#' then trees + 1 else trees
        countTrees right down newX newY newTrees

let result1 = countTrees 3 1 0 0 0
let result2 = countTrees 1 1 0 0 0
let result3 = countTrees 5 1 0 0 0
let result4 = countTrees 7 1 0 0 0
let result5 = countTrees 1 2 0 0 0

result1 * result2 * result3 * result4 * result5 |> printfn "%d"