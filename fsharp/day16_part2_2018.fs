
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Op = {
    Name:string
    Action:char
    A:char
    B:char
    MatchCount:ResizeArray<byte>
}

let runOp (op:Op) (regs:int[]) (instr:byte[]) =
    let r = Array.copy regs
    let a = if op.A='r' then r.[int instr.[1]] else int instr.[1]
    let b = if op.B='r' then r.[int instr.[2]] else int instr.[2]
    match op.Action with
    | '+' -> r.[int instr.[3]] <- a + b
    | '*' -> r.[int instr.[3]] <- a * b
    | '&' -> r.[int instr.[3]] <- a &&& b
    | '|' -> r.[int instr.[3]] <- a ||| b
    | 'a' -> r.[int instr.[3]] <- a
    | '>' -> r.[int instr.[3]] <- if a > b then 1 else 0
    | '=' -> r.[int instr.[3]] <- if a = b then 1 else 0
    | _ -> ()
    r

let matchRegs (e:int[]) (a:int[]) = e = a

let add (op:Op) (c:byte) =
    if not (op.MatchCount.Contains c) then op.MatchCount.Add c

let remove (op:Op) (c:byte) =
    let idx = op.MatchCount.IndexOf c
    if idx >= 0 then op.MatchCount.RemoveAt idx |> ignore

let testCode (regs:int[]) (result:int[]) (instr:byte[]) (ops:Op list) =
    let mutable cnt = 0
    for op in ops do
        let outRegs = runOp op regs instr
        if matchRegs result outRegs then
            add op instr.[0]
            cnt <- cnt + 1
    cnt

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines "input.txt"
    let opcodes = [
        {Name="addr"; Action='+'; A='r'; B='r'; MatchCount=ResizeArray()}
        {Name="addi"; Action='+'; A='r'; B='v'; MatchCount=ResizeArray()}
        {Name="mulr"; Action='*'; A='r'; B='r'; MatchCount=ResizeArray()}
        {Name="muli"; Action='*'; A='r'; B='v'; MatchCount=ResizeArray()}
        {Name="banr"; Action='&'; A='r'; B='r'; MatchCount=ResizeArray()}
        {Name="bani"; Action='&'; A='r'; B='v'; MatchCount=ResizeArray()}
        {Name="borr"; Action='|'; A='r'; B='r'; MatchCount=ResizeArray()}
        {Name="bori"; Action='|'; A='r'; B='v'; MatchCount=ResizeArray()}
        {Name="setr"; Action='a'; A='r'; B='r'; MatchCount=ResizeArray()}
        {Name="seti"; Action='a'; A='v'; B='r'; MatchCount=ResizeArray()}
        {Name="gtir"; Action='>'; A='v'; B='r'; MatchCount=ResizeArray()}
        {Name="gtri"; Action='>'; A='r'; B='v'; MatchCount=ResizeArray()}
        {Name="gtrr"; Action='>'; A='r'; B='r'; MatchCount=ResizeArray()}
        {Name="eqir"; Action='='; A='v'; B='r'; MatchCount=ResizeArray()}
        {Name="eqri"; Action='='; A='r'; B='v'; MatchCount=ResizeArray()}
        {Name="eqrr"; Action='='; A='r'; B='r'; MatchCount=ResizeArray()}
    ]
    let mutable i = 0
    while i < lines.Length && lines.[i].Length > 0 && lines.[i].[0] = 'B' do
        let split s = Regex.Split(s, "[^0-9]+") |> Array.filter ((<>) "")
        let regs = split lines.[i] |> Array.map int
        let instr = split lines.[i+1] |> Array.map byte
        let res = split lines.[i+2] |> Array.map int
        let cnt = testCode regs res instr opcodes
        i <- i + 4
    let ordered = Dictionary<byte, Op>()
    while ordered.Count < 16 do
        for op in opcodes do
            if op.MatchCount.Count = 1 then
                let c = op.MatchCount.[0]
                ordered.[c] <- op
                for op2 in opcodes do remove op2 c
    i <- i + 2
    let mutable regs = [|0;0;0;0|]
    while i < lines.Length do
        let split s = Regex.Split(s, "[^0-9]+") |> Array.filter ((<>) "")
        let instr = split lines.[i] |> Array.map byte
        regs <- runOp ordered.[instr.[0]] regs instr
        i <- i + 1
    printfn "%d" regs.[0]
    0
