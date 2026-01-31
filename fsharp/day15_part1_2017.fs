
open System
open System.IO

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let genAStart = int64 lines.[0]
    let genBStart = int64 lines.[1]

    let factorA = 16807L
    let factorB = 48271L
    let modulus = 2147483647L

    let mutable genA = genAStart
    let mutable genB = genBStart
    let mutable matches = 0

    for i = 1 to 40000000 do
        genA <- (genA * factorA) % modulus
        genB <- (genB * factorB) % modulus
        if (genA &&& 0xFFFFL) = (genB &&& 0xFFFFL) then
            matches <- matches + 1

    printfn "%d" matches
    0
