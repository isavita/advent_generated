
module Day5

let input = System.IO.File.ReadAllLines "input.txt" |> Array.map int

let rec solvePart1 (instructions:int[]) index steps =
    if index < 0 || index >= instructions.Length then steps
    else
        let offset = instructions.[index]
        instructions.[index] <- offset + 1
        solvePart1 instructions (index + offset) (steps + 1)

let resultPart1 = solvePart1 input 0 0
printfn "%d" resultPart1
