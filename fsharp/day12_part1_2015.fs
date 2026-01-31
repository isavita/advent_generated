
open System.IO

[<EntryPoint>]
let main _ =
    let text = File.ReadAllText "input.txt"
    let mutable sum = 0
    let mutable num = 0
    let mutable neg = false
    for c in text do
        if c = '-' then
            neg <- true
        elif c >= '0' && c <= '9' then
            num <- num * 10 + int (c - '0')
        else
            if neg then
                num <- -num
                neg <- false
            sum <- sum + num
            num <- 0
    if neg then num <- -num
    sum <- sum + num
    printfn "%d" sum
    0
