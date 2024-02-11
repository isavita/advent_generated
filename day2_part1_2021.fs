
module Day2

let lines = System.IO.File.ReadAllLines "input.txt"

let mutable horizontal = 0
let mutable depth = 0

for line in lines do
    let parts = line.Split(' ')
    let direction = parts.[0]
    let amount = int parts.[1]

    match direction with
    | "forward" -> horizontal <- horizontal + amount
    | "down" -> depth <- depth + amount
    | "up" -> depth <- depth - amount

let result = horizontal * depth
printfn "%d" result
