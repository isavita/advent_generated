
open System
open System.IO

let [<Literal>] MEM_SIZE = 16384
let [<Literal>] INPUT_BUFFER_SIZE = 256
let [<Literal>] OUTPUT_BUFFER_SIZE = 256

type IntcodeVM() =
    let code = Array.zeroCreate MEM_SIZE
    let inputBuffer = Array.zeroCreate INPUT_BUFFER_SIZE
    let outputBuffer = Array.zeroCreate OUTPUT_BUFFER_SIZE
    let mutable ip = 0L
    let mutable relativeBase = 0L
    let mutable inputHead = 0
    let mutable inputTail = 0
    let mutable outputHead = 0
    let mutable outputTail = 0
    let mutable halted = false

    member _.Load(filename: string) =
        File.ReadAllText(filename).Split(',')
        |> Array.map int64
        |> Array.iteri (fun i v -> code.[i] <- v)

    member _.SendString(s: string) =
        for c in s do
            inputBuffer.[inputTail] <- int64 c
            inputTail <- (inputTail + 1) % INPUT_BUFFER_SIZE
        inputBuffer.[inputTail] <- 10L
        inputTail <- (inputTail + 1) % INPUT_BUFFER_SIZE

    member _.ReadOutput() =
        while outputHead <> outputTail do
            let c = outputBuffer.[outputHead]
            outputHead <- (outputHead + 1) % OUTPUT_BUFFER_SIZE
            if c > 127L then
                printfn "%d" c

    member _.Run() =
        let inline getParamAddress pos mode =
            match mode with
            | 0L -> code.[int pos]
            | 1L -> pos
            | 2L -> relativeBase + code.[int pos]
            | _ -> failwith "Invalid parameter mode"
        let inline getValue pos mode = code.[int (getParamAddress pos mode)]
        while not halted do
            let instruction = code.[int ip]
            let opcode = instruction % 100L
            let mode1 = (instruction / 100L) % 10L
            let mode2 = (instruction / 1000L) % 10L
            let mode3 = (instruction / 10000L) % 10L
            match opcode with
            | 1L ->
                let p1 = getValue (ip+1L) mode1
                let p2 = getValue (ip+2L) mode2
                code.[int (getParamAddress (ip+3L) mode3)] <- p1 + p2
                ip <- ip + 4L
            | 2L ->
                let p1 = getValue (ip+1L) mode1
                let p2 = getValue (ip+2L) mode2
                code.[int (getParamAddress (ip+3L) mode3)] <- p1 * p2
                ip <- ip + 4L
            | 3L ->
                if inputHead = inputTail then ()
                else
                    code.[int (getParamAddress (ip+1L) mode1)] <- inputBuffer.[inputHead]
                    inputHead <- (inputHead + 1) % INPUT_BUFFER_SIZE
                    ip <- ip + 2L
            | 4L ->
                let p1 = getValue (ip+1L) mode1
                outputBuffer.[outputTail] <- p1
                outputTail <- (outputTail + 1) % OUTPUT_BUFFER_SIZE
                ip <- ip + 2L
            | 5L ->
                let p1 = getValue (ip+1L) mode1
                let p2 = getValue (ip+2L) mode2
                ip <- if p1 <> 0L then p2 else ip + 3L
            | 6L ->
                let p1 = getValue (ip+1L) mode1
                let p2 = getValue (ip+2L) mode2
                ip <- if p1 = 0L then p2 else ip + 3L
            | 7L ->
                let p1 = getValue (ip+1L) mode1
                let p2 = getValue (ip+2L) mode2
                code.[int (getParamAddress (ip+3L) mode3)] <- if p1 < p2 then 1L else 0L
                ip <- ip + 4L
            | 8L ->
                let p1 = getValue (ip+1L) mode1
                let p2 = getValue (ip+2L) mode2
                code.[int (getParamAddress (ip+3L) mode3)] <- if p1 = p2 then 1L else 0L
                ip <- ip + 4L
            | 9L ->
                relativeBase <- relativeBase + getValue (ip+1L) mode1
                ip <- ip + 2L
            | 99L -> halted <- true
            | _ -> failwithf "Invalid opcode: %d" opcode

[<EntryPoint>]
let main _ =
    let vm = IntcodeVM()
    vm.Load "input.txt"
    [|"NOT A J"; "NOT B T"; "OR T J"; "NOT C T"; "OR T J"; "AND D J"; "NOT A T"; "AND A T"; "OR E T"; "OR H T"; "AND T J"; "RUN"|]
    |> Array.iter vm.SendString
    vm.Run()
    vm.ReadOutput()
    0
