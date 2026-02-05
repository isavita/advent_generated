open System
open System.IO
open System.Collections.Generic

type VM(filename: string) =
    let code = Dictionary<int, int64>()
    do
        let text = File.ReadAllText(filename).Trim()
        let parts = text.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
        for i = 0 to parts.Length - 1 do
            code.[i] <- Int64.Parse(parts.[i])
    let input = Queue<int64>()
    let output = Queue<int64>()
    let mutable ip = 0
    let mutable relativeBase = 0

    member private this.GetValue(addr:int) : int64 =
        if code.ContainsKey(addr) then code.[addr] else 0L

    member private this.GetParamsAddresses(arity:int, instr:int64) : int [] =
        let modes = Array.zeroCreate<int> arity
        let mutable modeSection = instr / 100L
        for i = 0 to arity - 1 do
            modes.[i] <- int (modeSection % 10L)
            modeSection <- modeSection / 10L
        let addresses = Array.zeroCreate<int> arity
        for i = 0 to arity - 1 do
            let mode = modes.[i]
            let pos = ip + i + 1
            let addr =
                if mode = 0 then
                    int (this.GetValue(pos))
                elif mode = 1 then
                    pos
                else
                    relativeBase + int (this.GetValue(pos))
            addresses.[i] <- addr
        addresses

    member this.Run() =
        let mutable running = true
        while running do
            let instr = if code.ContainsKey(ip) then code.[ip] else 0L
            let opcode = int (instr % 100L)
            match opcode with
            | 1 ->
                let p = this.GetParamsAddresses(3, instr)
                code.[p.[2]] <- this.GetValue(p.[0]) + this.GetValue(p.[1])
                ip <- ip + 4
            | 2 ->
                let p = this.GetParamsAddresses(3, instr)
                code.[p.[2]] <- this.GetValue(p.[0]) * this.GetValue(p.[1])
                ip <- ip + 4
            | 3 ->
                let p = this.GetParamsAddresses(1, instr)
                code.[p.[0]] <- input.Dequeue()
                ip <- ip + 2
            | 4 ->
                let p = this.GetParamsAddresses(1, instr)
                output.Enqueue(this.GetValue(p.[0]))
                ip <- ip + 2
            | 5 ->
                let p = this.GetParamsAddresses(2, instr)
                if this.GetValue(p.[0]) <> 0L then
                    ip <- int (this.GetValue(p.[1]))
                else
                    ip <- ip + 3
            | 6 ->
                let p = this.GetParamsAddresses(2, instr)
                if this.GetValue(p.[0]) = 0L then
                    ip <- int (this.GetValue(p.[1]))
                else
                    ip <- ip + 3
            | 7 ->
                let p = this.GetParamsAddresses(3, instr)
                code.[p.[2]] <- if this.GetValue(p.[0]) < this.GetValue(p.[1]) then 1L else 0L
                ip <- ip + 4
            | 8 ->
                let p = this.GetParamsAddresses(3, instr)
                code.[p.[2]] <- if this.GetValue(p.[0]) = this.GetValue(p.[1]) then 1L else 0L
                ip <- ip + 4
            | 9 ->
                let p = this.GetParamsAddresses(1, instr)
                relativeBase <- relativeBase + int (this.GetValue(p.[0]))
                ip <- ip + 2
            | 99 -> running <- false
            | _ -> failwithf "Invalid opcode %d" opcode

    member this.SendString(s:string) =
        for ch in s do input.Enqueue(int64 (int ch))
        input.Enqueue(int64 10)

    member this.GetOutput() : int64 option =
        if output.Count = 0 then None else Some (output.Dequeue())

[<EntryPoint>]
let main argv =
    let vm = VM("input.txt")
    let instructions = [|
        "NOT A J"
        "NOT B T"
        "OR T J"
        "NOT C T"
        "OR T J"
        "AND D J"
        "WALK"
    |]
    for instr in instructions do vm.SendString(instr)
    vm.Run()
    let mutable finished = false
    while not finished do
        match vm.GetOutput() with
        | Some v when v > 127L ->
            printfn "%d" v
            finished <- true
        | Some _ -> ()
        | None -> finished <- true
    0