module Day3

let input = System.IO.File.ReadAllLines("input.txt")

let rec countTrees (x,y) (dx,dy) acc =
    if y >= input.Length then acc
    else
        let newX = (x + dx) % input.[0].Length
        let newY = y + dy
        let newAcc = if input.[y].[x] = '#' then acc + 1 else acc
        countTrees (newX,newY) (dx,dy) newAcc

let result = countTrees (0,0) (3,1) 0
printfn "%d" result