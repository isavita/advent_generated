
open System.IO

let lines = File.ReadAllLines "input.txt"
let genAStart = int64 lines.[0]
let genBStart = int64 lines.[1]

let genAFactor, genBFactor, modulus = 16807L, 48271L, 2147483647L

let rec loop genA genB matches left =
    if left = 0 then matches else
    let mutable a = (genA * genAFactor) % modulus
    while a % 4L <> 0L do
        a <- (a * genAFactor) % modulus
    let mutable b = (genB * genBFactor) % modulus
    while b % 8L <> 0L do
        b <- (b * genBFactor) % modulus
    let nextMatches = if (a &&& 0xFFFFL) = (b &&& 0xFFFFL) then matches + 1 else matches
    loop a b nextMatches (left - 1)

let answer = loop genAStart genBStart 0 5000000
printfn "%d" answer
