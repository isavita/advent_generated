
open System
open System.IO

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let mutable a = 1UL
    let mutable b = 0UL
    let mutable ip = 0
    let n = lines.Length
    while ip >= 0 && ip < n do
        let line = lines.[ip]
        match line.Substring(0,3) with
        | "hlf" ->
            let r = line.[4]
            if r = 'a' then a <- a / 2UL else b <- b / 2UL
            ip <- ip + 1
        | "tpl" ->
            let r = line.[4]
            if r = 'a' then a <- a * 3UL else b <- b * 3UL
            ip <- ip + 1
        | "inc" ->
            let r = line.[4]
            if r = 'a' then a <- a + 1UL else b <- b + 1UL
            ip <- ip + 1
        | "jmp" ->
            let offset = Int32.Parse(line.Substring(4).Trim())
            ip <- ip + offset
        | "jie" ->
            let r = line.[4]
            let offset = Int32.Parse(line.Substring(7).Trim())
            let v = if r = 'a' then a else b
            if v % 2UL = 0UL then ip <- ip + offset else ip <- ip + 1
        | "jio" ->
            let r = line.[4]
            let offset = Int32.Parse(line.Substring(7).Trim())
            let v = if r = 'a' then a else b
            if v = 1UL then ip <- ip + offset else ip <- ip + 1
        | _ -> ip <- ip + 1
    printfn "%d" b
    0
