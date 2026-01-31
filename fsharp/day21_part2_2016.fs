
open System
open System.IO

let swapPos (a: char[]) i j =
    let t = a.[i] in a.[i] <- a.[j]; a.[j] <- t

let swapLetter (a: char[]) x y =
    let i = Array.findIndex ((=) x) a
    let j = Array.findIndex ((=) y) a
    swapPos a i j

let rotate (a: char[]) steps =
    let n = a.Length
    let s = ((steps % n) + n) % n
    if s <> 0 then
        let tmp = Array.copy a
        for i in 0 .. n-1 do
            a.[i] <- tmp.[(i + n - s) % n]

let derotateLetter (a: char[]) x =
    let idx = Array.findIndex ((=) x) a
    let rot =
        if idx % 2 = 1 then -(idx + 1) / 2
        elif idx <> 0 then (6 - idx) / 2
        else -1
    rotate a rot

let reverse (a: char[]) i j =
    let mutable l = i
    let mutable r = j
    while l < r do
        swapPos a l r
        l <- l + 1
        r <- r - 1

let move (a: char[]) src dst =
    let ch = a.[src]
    if src < dst then
        Array.Copy(a, src + 1, a, src, dst - src)
    else
        Array.Copy(a, dst, a, dst + 1, src - dst)
    a.[dst] <- ch

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let mutable pw = "fbgdceah".ToCharArray()
    for instr in Array.rev lines do
        if instr.StartsWith "swap position" then
            let parts = instr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let x = int parts.[2]
            let y = int parts.[5]
            swapPos pw x y
        elif instr.StartsWith "swap letter" then
            let parts = instr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let x = parts.[2].[0]
            let y = parts.[5].[0]
            swapLetter pw x y
        elif instr.StartsWith "rotate based" then
            let x = instr.[instr.Length - 1]
            derotateLetter pw x
        elif instr.StartsWith "rotate " then
            let parts = instr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let dir = parts.[1]
            let steps = int parts.[2]
            let s = if dir = "left" then -steps else steps
            rotate pw (-s)
        elif instr.StartsWith "reverse" then
            let parts = instr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let x = int parts.[2]
            let y = int parts.[4]
            reverse pw x y
        elif instr.StartsWith "move" then
            let parts = instr.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let x = int parts.[2]
            let y = int parts.[5]
            move pw y x
    printfn "%s" (String(pw))
    0
