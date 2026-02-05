open System
open System.IO

let memorySize = 10000

let runProgram (program: int64[]) =
    let memory = Array.zeroCreate<int64>(memorySize)
    Array.Copy(program, memory, Math.Min(program.Length, memorySize))
    memory.[0] <- 2L

    let mutable ip = 0
    let mutable relBase = 0L
    let mutable ballX = 0L
    let mutable paddleX = 0L
    let mutable score = 0L
    let mutable halted = false
    let mutable tempX = 0L
    let mutable tempY = 0L
    let mutable outState = 0

    let inline inBounds idx = idx >= 0 && idx < memorySize

    let getAddr (offset:int) (mode:int) =
        let paramAddr = ip + offset
        if paramAddr < 0 || paramAddr >= memorySize then halted <- true; -1
        else
            let baseAddr64 =
                match mode with
                | 0 -> memory.[paramAddr]
                | 1 -> int64 paramAddr
                | 2 -> relBase + memory.[paramAddr]
                | _ -> halted <- true; -1L
            if baseAddr64 < 0L || baseAddr64 >= int64 memorySize then halted <- true; -1
            else int baseAddr64

    let getParam (offset:int) (mode:int) =
        let addr = getAddr offset mode
        if halted || addr = -1 then 0L
        else if mode = 1 then memory.[addr] else memory.[addr]

    let getWriteAddr (offset:int) (mode:int) =
        if mode = 1 then halted <- true; -1
        else getAddr offset mode

    while not halted do
        if ip < 0 || ip >= memorySize then halted <- true
        else
            let instruction = memory.[ip]
            let opcode = int (instruction % 100L)
            let modes = [| int ((instruction / 100L) % 10L)
                           int ((instruction / 1000L) % 10L)
                           int ((instruction / 10000L) % 10L) |]
            match opcode with
            | 1 | 2 ->
                let p1 = getParam 1 modes.[0]
                let p2 = getParam 2 modes.[1]
                let addr = getWriteAddr 3 modes.[2]
                if not halted && addr <> -1 then memory.[addr] <- (if opcode = 1 then p1 + p2 else p1 * p2)
                ip <- ip + 4
            | 3 ->
                let addr = getWriteAddr 1 modes.[0]
                if not halted && addr <> -1 then
                    let inputVal = if ballX > paddleX then 1L elif ballX < paddleX then -1L else 0L
                    memory.[addr] <- inputVal
                ip <- ip + 2
            | 4 ->
                let outputVal = getParam 1 modes.[0]
                if not halted then
                    if outState = 0 then tempX <- outputVal; outState <- 1
                    elif outState = 1 then tempY <- outputVal; outState <- 2
                    else
                        let tileId = outputVal
                        if tempX = -1L && tempY = 0L then
                            score <- tileId
                        else
                            if tileId = 3L then paddleX <- tempX
                            elif tileId = 4L then ballX <- tempX
                        outState <- 0
                ip <- ip + 2
            | 5 ->
                let p1 = getParam 1 modes.[0]
                let p2 = getParam 2 modes.[1]
                if not halted then
                    if p1 <> 0L then
                        if p2 < 0L || p2 >= int64 memorySize then halted <- true
                        else ip <- int p2
                    else ip <- ip + 3
            | 6 ->
                let p1 = getParam 1 modes.[0]
                let p2 = getParam 2 modes.[1]
                if not halted then
                    if p1 = 0L then
                        if p2 < 0L || p2 >= int64 memorySize then halted <- true
                        else ip <- int p2
                    else ip <- ip + 3
            | 7 ->
                let p1 = getParam 1 modes.[0]
                let p2 = getParam 2 modes.[1]
                let addr = getWriteAddr 3 modes.[2]
                if not halted && addr <> -1 then memory.[addr] <- (if p1 < p2 then 1L else 0L)
                ip <- ip + 4
            | 8 ->
                let p1 = getParam 1 modes.[0]
                let p2 = getParam 2 modes.[1]
                let addr = getWriteAddr 3 modes.[2]
                if not halted && addr <> -1 then memory.[addr] <- (if p1 = p2 then 1L else 0L)
                ip <- ip + 4
            | 9 ->
                let p1 = getParam 1 modes.[0]
                relBase <- relBase + p1
                ip <- ip + 2
            | 99 -> halted <- true
            | _ -> halted <- true

    score

[<EntryPoint>]
let main argv =
    if not (File.Exists("input.txt")) then 1
    else
        let text = File.ReadAllText("input.txt")
        let program = text.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
                      |> Array.map (fun s -> Int64.Parse(s.Trim()))
        printfn "%d" (runProgram program)
        0