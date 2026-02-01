
open System
open System.IO
open System.Text

let getComboVal op a b c =
    match op with
    | 1 -> 1
    | 2 -> 2
    | 3 -> 3
    | 4 -> a
    | 5 -> b
    | 6 -> c
    | _ -> 0

[<EntryPoint>]
let main _ =
    let mutable a = 0
    let mutable b = 0
    let mutable c = 0
    let program = ResizeArray<int>()
    let output = StringBuilder()
    for line in File.ReadLines("input.txt") do
        let line = line.Trim()
        if line.StartsWith("Register A:") then
            a <- Int32.Parse(line.Substring(11).Trim())
        elif line.StartsWith("Register B:") then
            b <- Int32.Parse(line.Substring(11).Trim())
        elif line.StartsWith("Register C:") then
            c <- Int32.Parse(line.Substring(11).Trim())
        elif line.StartsWith("Program:") then
            let parts = line.Substring(8).Trim().Split([|','|], StringSplitOptions.RemoveEmptyEntries)
            for p in parts do program.Add(Int32.Parse(p.Trim()))
    let mutable ip = 0
    while ip < program.Count do
        let opcode = program.[ip]
        if ip + 1 >= program.Count then ip <- ip + 1 else
        let operand = program.[ip + 1]
        match opcode with
        | 0 ->
            let den = getComboVal operand a b c
            a <- if den <> 0 then a / (1 <<< den) else 0
            ip <- ip + 2
        | 1 ->
            b <- b ^^^ operand
            ip <- ip + 2
        | 2 ->
            b <- getComboVal operand a b c % 8
            ip <- ip + 2
        | 3 ->
            ip <- if a <> 0 then operand else ip + 2
        | 4 ->
            b <- b ^^^ c
            ip <- ip + 2
        | 5 ->
            let v = getComboVal operand a b c % 8
            output.Append(v).Append(',') |> ignore
            ip <- ip + 2
        | 6 ->
            let den = getComboVal operand a b c
            b <- a / (if den <> 0 then 1 <<< den else 1)
            ip <- ip + 2
        | 7 ->
            let den = getComboVal operand a b c
            c <- a / (if den <> 0 then 1 <<< den else 1)
            ip <- ip + 2
        | _ -> ip <- ip + 1
    if output.Length > 0 then output.Length <- output.Length - 1
    Console.WriteLine(output.ToString())
    0
