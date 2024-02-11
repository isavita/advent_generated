
module Day5

let input = System.IO.File.ReadAllText "input.txt"

let parseInput (input:string) =
    input.Split(',') 
    |> Array.map int

let rec runProgram (program:int[]) (index:int) =
    let getValue (mode:int) (param:int) =
        if mode = 0 then program.[param]
        else param

    let getParams (opcode:int) =
        let mode1 = (opcode / 100) % 10
        let mode2 = (opcode / 1000) % 10
        let param1 = program.[index+1]
        let param2 = program.[index+2]
        let param3 = program.[index+3]
        (getValue mode1 param1, getValue mode2 param2, param3)

    match program.[index] % 100 with
    | 1 -> 
        let (param1, param2, param3) = getParams program.[index]
        program.[param3] <- param1 + param2
        runProgram program (index + 4)
    | 2 -> 
        let (param1, param2, param3) = getParams program.[index]
        program.[param3] <- param1 * param2
        runProgram program (index + 4)
    | 3 -> 
        program.[program.[index+1]] <- 1
        runProgram program (index + 2)
    | 4 -> 
        printfn "%d" program.[program.[index+1]]
        runProgram program (index + 2)
    | 99 -> program.[0]
    | _ -> failwith "Invalid opcode"

let program = parseInput input
runProgram program 0
