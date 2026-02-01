
open System
open System.IO
open System.Text.RegularExpressions

type Op = { Name:string; Action:char; A:char; B:char }

let opcodes = [
    {Name="addr"; Action='+' ; A='r'; B='r'}
    {Name="addi"; Action='+' ; A='r'; B='v'}
    {Name="mulr"; Action='*' ; A='r'; B='r'}
    {Name="muli"; Action='*' ; A='r'; B='v'}
    {Name="banr"; Action='&' ; A='r'; B='r'}
    {Name="bani"; Action='&' ; A='r'; B='v'}
    {Name="borr"; Action='|' ; A='r'; B='r'}
    {Name="bori"; Action='|' ; A='r'; B='v'}
    {Name="setr"; Action='a' ; A='r'; B='r'}
    {Name="seti"; Action='a' ; A='v'; B='r'}
    {Name="gtir"; Action='>' ; A='v'; B='r'}
    {Name="gtri"; Action='>' ; A='r'; B='v'}
    {Name="gtrr"; Action='>' ; A='r'; B='r'}
    {Name="eqir"; Action='=' ; A='v'; B='r'}
    {Name="eqri"; Action='=' ; A='r'; B='v'}
    {Name="eqrr"; Action='=' ; A='r'; B='r'}
]

let runOp op (reg:int[]) (inst:byte[]) =
    let reg' = Array.copy reg
    let a = if op.A='r' then reg'.[int inst.[1]] else int inst.[1]
    let b = if op.B='r' then reg'.[int inst.[2]] else int inst.[2]
    reg'.[int inst.[3]] <-
        match op.Action with
        | '+' -> a + b
        | '*' -> a * b
        | '&' -> a &&& b
        | '|' -> a ||| b
        | 'a' -> a
        | '>' -> if a > b then 1 else 0
        | '=' -> if a = b then 1 else 0
        | _   -> failwith "bad op"
    reg'

let testCode reg n inst =
    opcodes |> List.fold (fun cnt op ->
        if runOp op reg inst = n then cnt + 1 else cnt) 0

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let mutable i = 0
    let mutable sum = 0
    while i < lines.Length && lines.[i].StartsWith "Before" do
        let parseInts (s:string) =
            Regex.Matches(s, @"\d+") |> Seq.map (fun m -> int m.Value) |> Seq.toArray
        let before = parseInts lines.[i]
        let inst = parseInts lines.[i+1] |> Array.map byte
        let after  = parseInts lines.[i+2]
        if testCode before after inst >= 3 then sum <- sum + 1
        i <- i + 4
    printfn "%d" sum
    0
