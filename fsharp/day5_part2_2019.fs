
open System
open System.IO

let getValue (program: int array) pos mode =
    if mode = 0 then program.[program.[pos]] else program.[pos]

let executeProgram (program: int array) inputValue =
    let mutable i = 0
    let mutable output = 0
    while true do
        let opcode = program.[i] % 100
        let modes = program.[i] / 100
        let param1Mode = modes % 10
        let param2Mode = (modes / 10) % 10
        match opcode with
        | 1 ->
            let p1 = getValue program (i + 1) param1Mode
            let p2 = getValue program (i + 2) param2Mode
            let p3 = program.[i + 3]
            program.[p3] <- p1 + p2
            i <- i + 4
        | 2 ->
            let p1 = getValue program (i + 1) param1Mode
            let p2 = getValue program (i + 2) param2Mode
            let p3 = program.[i + 3]
            program.[p3] <- p1 * p2
            i <- i + 4
        | 3 ->
            program.[program.[i + 1]] <- inputValue
            i <- i + 2
        | 4 ->
            output <- getValue program (i + 1) param1Mode
            printfn "%d" output
            i <- i + 2
        | 5 ->
            let p1 = getValue program (i + 1) param1Mode
            let p2 = getValue program (i + 2) param2Mode
            if p1 <> 0 then i <- p2 else i <- i + 3
        | 6 ->
            let p1 = getValue program (i + 1) param1Mode
            let p2 = getValue program (i + 2) param2Mode
            if p1 = 0 then i <- p2 else i <- i + 3
        | 7 ->
            let p1 = getValue program (i + 1) param1Mode
            let p2 = getValue program (i + 2) param2Mode
            let p3 = program.[i + 3]
            program.[p3] <- if p1 < p2 then 1 else 0
            i <- i + 4
        | 8 ->
            let p1 = getValue program (i + 1) param1Mode
            let p2 = getValue program (i + 2) param2Mode
            let p3 = program.[i + 3]
            program.[p3] <- if p1 = p2 then 1 else 0
            i <- i + 4
        | 99 -> exit 0
        | _ -> failwith "Invalid opcode"

let main () =
    try
        let input = File.ReadAllText("input.txt").Trim()
        let program = input.Split(',') |> Array.map int
        executeProgram program 5
    with
    | ex -> printfn "Error: %s" ex.Message; exit 1

main ()
