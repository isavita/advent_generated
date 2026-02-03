
open System
open System.IO

[<EntryPoint>]
let main _ =
    let MAX_CUPS = 1_000_000
    let cups = Array.zeroCreate<int> (MAX_CUPS + 1)
    let input = File.ReadAllText("input.txt").Trim()
    let len = input.Length
    let mutable current = int input.[0] - int '0'
    for i in 0 .. len - 1 do
        let cup = int input.[i] - int '0'
        cups.[cup] <- if i < len - 1 then int input.[i + 1] - int '0' else int input.[0] - int '0'
    for _ in 0 .. 99 do
        let p1 = cups.[current]
        let p2 = cups.[p1]
        let p3 = cups.[p2]
        cups.[current] <- cups.[p3]
        let mutable dest = current - 1
        if dest < 1 then dest <- len
        while dest = p1 || dest = p2 || dest = p3 do
            dest <- dest - 1
            if dest < 1 then dest <- len
        cups.[p3] <- cups.[dest]
        cups.[dest] <- p1
        current <- cups.[current]
    let mutable c = cups.[1]
    while c <> 1 do
        printf "%d" c
        c <- cups.[c]
    printfn ""
    0
