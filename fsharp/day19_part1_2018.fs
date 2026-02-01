
open System
open System.IO

let lines = File.ReadAllLines "input.txt"
let ipBind = int (lines.[0].Split()[1])
let prog = 
    lines.[1..] 
    |> Array.map (fun s -> 
        let a = s.Split()
        (a.[0], int a.[1], int a.[2], int a.[3]))

let mutable ip = 0
let r = Array.zeroCreate 6

while ip >= 0 && ip < prog.Length do
    r.[ipBind] <- ip
    let op,a,b,c = prog.[ip]
    match op with
    | "addr" -> r.[c] <- r.[a] + r.[b]
    | "addi" -> r.[c] <- r.[a] + b
    | "mulr" -> r.[c] <- r.[a] * r.[b]
    | "muli" -> r.[c] <- r.[a] * b
    | "banr" -> r.[c] <- r.[a] &&& r.[b]
    | "bani" -> r.[c] <- r.[a] &&& b
    | "borr" -> r.[c] <- r.[a] ||| r.[b]
    | "bori" -> r.[c] <- r.[a] ||| b
    | "setr" -> r.[c] <- r.[a]
    | "seti" -> r.[c] <- a
    | "gtir" -> r.[c] <- if a > r.[b] then 1 else 0
    | "gtri" -> r.[c] <- if r.[a] > b then 1 else 0
    | "gtrr" -> r.[c] <- if r.[a] > r.[b] then 1 else 0
    | "eqir" -> r.[c] <- if a = r.[b] then 1 else 0
    | "eqri" -> r.[c] <- if r.[a] = b then 1 else 0
    | "eqrr" -> r.[c] <- if r.[a] = r.[b] then 1 else 0
    | _ -> ()
    ip <- r.[ipBind] + 1

printfn "%d" r.[0]
