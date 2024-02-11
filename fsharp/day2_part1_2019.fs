
module Day2

let input = System.IO.File.ReadAllText "input.txt"
let program = input.Split(',') |> Array.map int

let rec runProgram (arr:int[]) pos =
    match arr.[pos] with
    | 1 -> 
        let index1 = arr.[pos+1]
        let index2 = arr.[pos+2]
        let index3 = arr.[pos+3]
        arr.[index3] <- arr.[index1] + arr.[index2]
        runProgram arr (pos+4)
    | 2 -> 
        let index1 = arr.[pos+1]
        let index2 = arr.[pos+2]
        let index3 = arr.[pos+3]
        arr.[index3] <- arr.[index1] * arr.[index2]
        runProgram arr (pos+4)
    | 99 -> arr.[0]
    | _ -> failwith "Unknown opcode"

program.[1] <- 12
program.[2] <- 2

let result = runProgram program 0
printfn "%d" result
