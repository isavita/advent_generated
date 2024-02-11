module Day2

let input = System.IO.File.ReadAllText("input.txt")

let parseInput (input:string) =
    input.Split(',')
    |> Array.map int

let rec runProgram (program:int[]) (pos:int) =
    match program.[pos] with
    | 1 ->
        let index1 = program.[pos+1]
        let index2 = program.[pos+2]
        let index3 = program.[pos+3]
        program.[index3] <- program.[index1] + program.[index2]
        runProgram program (pos+4)
    | 2 ->
        let index1 = program.[pos+1]
        let index2 = program.[pos+2]
        let index3 = program.[pos+3]
        program.[index3] <- program.[index1] * program.[index2]
        runProgram program (pos+4)
    | 99 -> program.[0]
    | _ -> failwith "Invalid opcode"

let findOutput (noun:int) (verb:int) =
    let program = parseInput input
    program.[1] <- noun
    program.[2] <- verb
    runProgram program 0

let rec findInputs (noun:int) (verb:int) =
    if findOutput noun verb = 19690720 then
        100 * noun + verb
    else if verb < 99 then
        findInputs noun (verb + 1)
    else
        findInputs (noun + 1) 0

let result = findInputs 0 0

printfn "%A" result