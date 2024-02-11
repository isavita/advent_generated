
module Day12

let rec runProgram (registers: Map<string, int>) (instructions: string list) (index: int) : int =
    if index >= List.length instructions then
        registers.["a"]
    else
        let parts = instructions.[index].Split(' ')
        match parts.[0] with
        | "cpy" -> 
            let mutable value = 0
            if System.Int32.TryParse(parts.[1], &value) then
                let newRegisters = registers.Add(parts.[2], value)
                runProgram newRegisters instructions (index + 1)
            else
                let value = registers.[parts.[1]]
                let newRegisters = registers.Add(parts.[2], value)
                runProgram newRegisters instructions (index + 1)
        | "inc" -> 
            let newRegisters = registers.Add(parts.[1], registers.[parts.[1]] + 1)
            runProgram newRegisters instructions (index + 1)
        | "dec" -> 
            let newRegisters = registers.Add(parts.[1], registers.[parts.[1]] - 1)
            runProgram newRegisters instructions (index + 1)
        | "jnz" -> 
            let mutable check = 0
            if System.Int32.TryParse(parts.[1], &check) then
                if check <> 0 then
                    runProgram registers instructions (index + int parts.[2])
                else
                    runProgram registers instructions (index + 1)
            else
                let check = registers.[parts.[1]]
                if check <> 0 then
                    runProgram registers instructions (index + int parts.[2])
                else
                    runProgram registers instructions (index + 1)
        | _ -> failwith "Invalid instruction"

let input = System.IO.File.ReadAllLines "input.txt" |> List.ofArray
let initialRegisters = Map.ofList ["a", 0; "b", 0; "c", 0; "d", 0]
let resultPart1 = runProgram initialRegisters input 0
let resultPart2 = runProgram (initialRegisters.Add("c", 1)) input 0

printfn "%d" resultPart1
printfn "%d" resultPart2
