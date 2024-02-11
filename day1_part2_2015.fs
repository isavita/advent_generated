module Day1

let input = System.IO.File.ReadAllText("input.txt")

let mutable floor = 0
let mutable position = 0
let mutable basementFound = false

for c in input do
    position <- position + 1
    if c = '(' then
        floor <- floor + 1
    elif c = ')' then
        floor <- floor - 1
    if not basementFound && floor = -1 then
        basementFound <- true
        printfn "%d" position

printfn "%d" floor