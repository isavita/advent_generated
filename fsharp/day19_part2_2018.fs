
open System
open System.IO

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let mutable ipr = 0
    let prog = [|
        for l in lines do
            let p = l.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            if l.StartsWith("#ip") then ipr <- int p.[1]
            elif p.Length = 4 then yield (p.[0], int p.[1], int p.[2], int p.[3])
    |]
    let r = [| 1; 0; 0; 0; 0; 0 |]
    let mutable ip, cyc = 0, 0
    while ip >= 0 && ip < prog.Length && cyc < 1000 do
        r.[ipr] <- ip
        let (op, a, b, res) = prog.[ip]
        r.[res] <- match op with
                    | "addr" -> r.[a] + r.[b]
                    | "addi" -> r.[a] + b
                    | "mulr" -> r.[a] * r.[b]
                    | "muli" -> r.[a] * b
                    | "banr" -> r.[a] &&& r.[b]
                    | "bani" -> r.[a] &&& b
                    | "borr" -> r.[a] ||| r.[b]
                    | "bori" -> r.[a] ||| b
                    | "setr" -> r.[a]
                    | "seti" -> a
                    | "gtir" -> if a > r.[b] then 1 else 0
                    | "gtri" -> if r.[a] > b then 1 else 0
                    | "gtrr" -> if r.[a] > r.[b] then 1 else 0
                    | "eqir" -> if a = r.[b] then 1 else 0
                    | "eqri" -> if r.[a] = b then 1 else 0
                    | "eqrr" -> if r.[a] = r.[b] then 1 else 0
                    | _ -> 0
        ip <- r.[ipr] + 1
        cyc <- cyc + 1
    let n = Array.max r
    let mutable t = 0
    let mutable i = 1
    while i * i <= n do
        if n % i = 0 then
            t <- t + i
            if i * i <> n then t <- t + (n / i)
        i <- i + 1
    printfn "%d" t
    0

