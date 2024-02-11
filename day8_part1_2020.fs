module Day8

let input = System.IO.File.ReadAllLines "input.txt"

let rec runProgram (instructions:string array) (index:int) (acc:int) (visited:int list) =
    if List.contains index visited then acc
    else
        let newVisited = index :: visited
        let instruction = instructions.[index]
        let parts = instruction.Split(' ')
        let op = parts.[0]
        let arg = int parts.[1]
        match op with
        | "nop" -> runProgram instructions (index + 1) acc newVisited
        | "acc" -> runProgram instructions (index + 1) (acc + arg) newVisited
        | "jmp" -> runProgram instructions (index + arg) acc newVisited

let result = runProgram input 0 0 [] 
printfn "%d" result