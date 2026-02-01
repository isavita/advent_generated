
open System
open System.IO
open System.Collections.Generic

let mem = Dictionary<int64,int64>()
let mutable relativeBase = 0L

let read addr =
    if addr < 0L then failwith "Negative address"
    match mem.TryGetValue addr with
    | true,v -> v
    | _ -> 0L

let write addr value = mem.[addr] <- value

let getParam mode pos =
    let param = read pos
    match mode with
    | 0 -> read param
    | 1 -> param
    | 2 -> read (param + relativeBase)
    | _ -> failwith "Invalid parameter mode"

let setParam mode pos value =
    let param = read pos
    match mode with
    | 0 -> write param value
    | 2 -> write (param + relativeBase) value
    | _ -> failwith "Invalid parameter mode for setting"

let runIntcode input =
    let mutable ip = 0L
    let mutable output = 0L
    let mutable running = true
    while running do
        let instr = read ip
        let opcode = instr % 100L
        let mode1 = int (instr / 100L % 10L)
        let mode2 = int (instr / 1000L % 10L)
        let mode3 = int (instr / 10000L % 10L)
        match opcode with
        | 99L -> running <- false
        | 1L ->
            let v = getParam mode1 (ip+1L) + getParam mode2 (ip+2L)
            setParam mode3 (ip+3L) v
            ip <- ip + 4L
        | 2L ->
            let v = getParam mode1 (ip+1L) * getParam mode2 (ip+2L)
            setParam mode3 (ip+3L) v
            ip <- ip + 4L
        | 3L ->
            setParam mode1 (ip+1L) input
            ip <- ip + 2L
        | 4L ->
            output <- getParam mode1 (ip+1L)
            printfn "%d" output
            ip <- ip + 2L
        | 5L ->
            if getParam mode1 (ip+1L) <> 0L then
                ip <- getParam mode2 (ip+2L)
            else
                ip <- ip + 3L
        | 6L ->
            if getParam mode1 (ip+1L) = 0L then
                ip <- getParam mode2 (ip+2L)
            else
                ip <- ip + 3L
        | 7L ->
            let v = if getParam mode1 (ip+1L) < getParam mode2 (ip+2L) then 1L else 0L
            setParam mode3 (ip+3L) v
            ip <- ip + 4L
        | 8L ->
            let v = if getParam mode1 (ip+1L) = getParam mode2 (ip+2L) then 1L else 0L
            setParam mode3 (ip+3L) v
            ip <- ip + 4L
        | 9L ->
            relativeBase <- relativeBase + getParam mode1 (ip+1L)
            ip <- ip + 2L
        | _ -> failwithf "Invalid opcode %d" opcode
    output

[<EntryPoint>]
let main argv =
    let content = File.ReadAllText("input.txt").Trim()
    let tokens = content.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
    mem.Clear()
    tokens |> Array.iteri (fun i tok -> mem.[int64 i] <- int64 tok)

    relativeBase <- 0L
    runIntcode 1L |> ignore

    relativeBase <- 0L
    runIntcode 2L |> ignore
    0
