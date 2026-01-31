
open System
open System.IO

type Instruction = { mutable Op: string; Arg1: string; Arg2: string }

let getValue (s: string) (regs: int[]) =
    if Char.IsLetter s.[0] then regs.[int s.[0] - int 'a']
    else Int32.Parse s

let toggle (instr: Instruction) =
    match instr.Op with
    | "inc" -> instr.Op <- "dec"
    | "dec" | "tgl" -> instr.Op <- "inc"
    | "jnz" -> instr.Op <- "cpy"
    | "cpy" -> instr.Op <- "jnz"
    | _ -> ()

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let instrs = 
        lines 
        |> Array.map (fun l ->
            let parts = l.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            { Op = parts.[0]; Arg1 = parts.[1]; Arg2 = if parts.Length > 2 then parts.[2] else "" })
        |> Array.copy

    let regs = Array.zeroCreate<int> 26
    regs.[0] <- 7   // a

    let mutable pc = 0
    while pc < instrs.Length do
        let i = instrs.[pc]
        match i.Op with
        | "cpy" ->
            if Char.IsLetter i.Arg2.[0] then
                regs.[int i.Arg2.[0] - int 'a'] <- getValue i.Arg1 regs
        | "inc" ->
            if Char.IsLetter i.Arg1.[0] then
                regs.[int i.Arg1.[0] - int 'a'] <- regs.[int i.Arg1.[0] - int 'a'] + 1
        | "dec" ->
            if Char.IsLetter i.Arg1.[0] then
                regs.[int i.Arg1.[0] - int 'a'] <- regs.[int i.Arg1.[0] - int 'a'] - 1
        | "jnz" ->
            if getValue i.Arg1 regs <> 0 then
                pc <- pc + getValue i.Arg2 regs - 1
        | "tgl" ->
            let tgt = pc + getValue i.Arg1 regs
            if tgt >= 0 && tgt < instrs.Length then toggle instrs.[tgt]
        | _ -> ()
        pc <- pc + 1

    printfn "%d" regs.[0]
    0
