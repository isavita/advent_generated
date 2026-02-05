
open System
open System.IO
open System.Collections.Generic

type Computer(program: int64[]) =
    let mem = Array.zeroCreate<int64> 100000
    do Array.blit program 0 mem 0 program.Length
    let mutable ip = 0L
    let mutable rb = 0L
    let input = Queue<int64>()
    let output = Queue<int64>()
    let mutable halted = false
    let mutable needsInput = false

    let getAddr mode offset =
        let v = mem.[int(ip + offset)]
        match mode with
        | 0 -> int v
        | 1 -> int(ip + offset)
        | 2 -> int(rb + v)
        | _ -> failwith ""

    member _.Input = input
    member _.Output = output
    member _.Halted = halted
    member _.NeedsInput = needsInput

    member this.Run() =
        needsInput <- false
        let mutable suspended = false
        while not halted && not suspended do
            let instr = mem.[int ip]
            let opcode = int(instr % 100L)
            let m1 = int(instr / 100L % 10L)
            let m2 = int(instr / 1000L % 10L)
            let m3 = int(instr / 10000L % 10L)
            match opcode with
            | 1 ->
                mem.[getAddr m3 3L] <- mem.[getAddr m1 1L] + mem.[getAddr m2 2L]
                ip <- ip + 4L
            | 2 ->
                mem.[getAddr m3 3L] <- mem.[getAddr m1 1L] * mem.[getAddr m2 2L]
                ip <- ip + 4L
            | 3 ->
                if input.Count = 0 then
                    needsInput <- true
                    suspended <- true
                else
                    mem.[getAddr m1 1L] <- input.Dequeue()
                    ip <- ip + 2L
            | 4 ->
                output.Enqueue(mem.[getAddr m1 1L])
                ip <- ip + 2L
                if output.Count = 3 then suspended <- true
            | 5 ->
                if mem.[getAddr m1 1L] <> 0L then ip <- mem.[getAddr m2 2L] else ip <- ip + 3L
            | 6 ->
                if mem.[getAddr m1 1L] = 0L then ip <- mem.[getAddr m2 2L] else ip <- ip + 3L
            | 7 ->
                mem.[getAddr m3 3L] <- if mem.[getAddr m1 1L] < mem.[getAddr m2 2L] then 1L else 0L
                ip <- ip + 4L
            | 8 ->
                mem.[getAddr m3 3L] <- if mem.[getAddr m1 1L] = mem.[getAddr m2 2L] then 1L else 0L
                ip <- ip + 4L
            | 9 ->
                rb <- rb + mem.[getAddr m1 1L]
                ip <- ip + 2L
            | 99 -> halted <- true
            | _ -> suspended <- true

[<EntryPoint>]
let main _ =
    let inputStr = File.ReadAllText("input.txt")
    let program = inputStr.Split([|','; ' '; '\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int64
    let computers = Array.init 50 (fun i ->
        let c = Computer(program)
        c.Input.Enqueue(int64 i)
        c
    )
    let packetQueues = Array.init 50 (fun _ -> Queue<int64 * int64>())
    while true do
        for i in 0 .. 49 do
            let c = computers.[i]
            if c.NeedsInput && c.Input.Count = 0 then
                if packetQueues.[i].Count > 0 then
                    let x, y = packetQueues.[i].Dequeue()
                    c.Input.Enqueue x; c.Input.Enqueue y
                else
                    c.Input.Enqueue -1L
            c.Run()
            while c.Output.Count >= 3 do
                let dest = int (c.Output.Dequeue())
                let x, y = c.Output.Dequeue(), c.Output.Dequeue()
                if dest = 255 then
                    printfn "%d" y
                    exit 0
                elif dest >= 0 && dest < 50 then
                    packetQueues.[dest].Enqueue((x, y))
    0

