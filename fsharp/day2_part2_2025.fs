
open System
open System.IO

let isInvalid (x:uint64) =
    let s = x.ToString()
    if s.Length <= 1 then false else
    let rec loop p =
        if p > s.Length/2 then false else
        if s.Length % p <> 0 then loop (p+1) else
        let k = s.Length / p
        if k < 2 then loop (p+1) else
        let rec check i =
            if i = s.Length then true
            elif s.[i] <> s.[i%p] then false
            else check (i+1)
        if check p then true else loop (p+1)
    loop 1

[<EntryPoint>]
let main _ =
    let txt = File.ReadAllText "input.txt"
    let mutable p = 0
    let mutable sum = 0UL
    while p < txt.Length do
        while p < txt.Length && " \n\r\t,".Contains(txt.[p]) do p <- p + 1
        if p < txt.Length then
            let astart = p
            while p < txt.Length && Char.IsDigit txt.[p] do p <- p + 1
            let a = uint64 (txt.Substring(astart, p-astart))
            if p < txt.Length && txt.[p] = '-' then
                p <- p + 1
                let bstart = p
                while p < txt.Length && Char.IsDigit txt.[p] do p <- p + 1
                let b = uint64 (txt.Substring(bstart, p-bstart))
                let a, b = if a > b then b, a else a, b
                let mutable x = a
                while x <= b do
                    if isInvalid x then sum <- sum + x
                    if x = UInt64.MaxValue then () else x <- x + 1UL
    printfn "%d" sum
    0
