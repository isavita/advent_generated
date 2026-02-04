
open System
open System.IO

type Instruction = {
    mutable Op: string
    mutable Arg1Type: int
    mutable Arg1Val: int
    mutable Arg2Type: int
    mutable Arg2Val: int
}

let isReg (s:string) = s.Length = 1 && s.[0] >= 'a' && s.[0] <= 'd'
let regIdx (c:char) = int c - int 'a'

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let n = lines.Length
    let instr = Array.zeroCreate<Instruction> n
    for i in 0 .. n-1 do
        let parts = lines.[i].Split([|' ';'\t'|], StringSplitOptions.RemoveEmptyEntries)
        let op = parts.[0]
        let mutable a1t = 0
        let mutable a1v = 0
        let mutable a2t = 0
        let mutable a2v = 0
        if parts.Length > 1 then
            if isReg parts.[1] then a1t <- 0; a1v <- regIdx parts.[1].[0] else a1t <- 1; a1v <- int parts.[1]
            if parts.Length > 2 then
                if isReg parts.[2] then a2t <- 0; a2v <- regIdx parts.[2].[0] else a2t <- 1; a2v <- int parts.[2]
        instr.[i] <- { Op = op; Arg1Type = a1t; Arg1Val = a1v; Arg2Type = a2t; Arg2Val = a2v }

    let reg = [|12;0;0;0|]
    let mutable pc = 0
    while pc < n do
        let cur = instr.[pc]
        let val1 = if cur.Arg1Type = 0 then reg.[cur.Arg1Val] else cur.Arg1Val
        let mutable jumped = false
        match cur.Op with
        | "cpy" ->
            reg.[cur.Arg2Val] <- val1
        | "inc" ->
            reg.[cur.Arg1Val] <- reg.[cur.Arg1Val] + 1
        | "dec" ->
            reg.[cur.Arg1Val] <- reg.[cur.Arg1Val] - 1
        | "jnz" ->
            if val1 <> 0 then
                let offset = if cur.Arg2Type = 0 then reg.[cur.Arg2Val] else cur.Arg2Val
                pc <- pc + offset
                jumped <- true
        | "tgl" ->
            let target = pc + val1
            if target >= 0 && target < n then
                let tgt = instr.[target]
                match tgt.Op with
                | "inc" -> tgt.Op <- "dec"
                | "dec" | "tgl" -> tgt.Op <- "inc"
                | "jnz" -> tgt.Op <- "cpy"
                | "cpy" -> tgt.Op <- "jnz"
                | _ -> ()
        | _ -> ()
        if not jumped then pc <- pc + 1
    printfn "%d" reg.[0]
    0
