
open System
open System.IO

// Modular addition: (a + b) % m
let modAdd (a: uint64) (b: uint64) (m: uint64) =
    let a = a % m
    let b = b % m
    let res = a + b
    if res < a || res >= m then res - m else res

// Modular subtraction: (a - b) % m
let modSub (a: uint64) (b: uint64) (m: uint64) =
    let a = a % m
    let b = b % m
    (a + m - b) % m

// Modular multiplication: (a * b) % m
let modMul (a: uint64) (b: uint64) (m: uint64) =
    let mutable res = 0UL
    let mutable a = a % m
    let mutable b = b
    while b > 0UL do
        if (b &&& 1UL) = 1UL then res <- modAdd res a m
        a <- modAdd a a m
        b <- b >>> 1
    res

// Modular exponentiation: (base^exp) % mod
let modPow (baseVal: uint64) (exp: uint64) (modVal: uint64) =
    let mutable res = 1UL
    let mutable baseVal = baseVal % modVal
    let mutable exp = exp
    while exp > 0UL do
        if (exp &&& 1UL) = 1UL then res <- modMul res baseVal modVal
        baseVal <- modMul baseVal baseVal modVal
        exp <- exp >>> 1
    res

// Modular multiplicative inverse: n^-1 % mod
let modInverse (n: uint64) (modVal: uint64) =
    modPow n (modVal - 2UL) modVal

let [<Literal>] SIZE = 119315717514047UL
let [<Literal>] ITERATIONS = 101741582076661UL
let [<Literal>] TARGET_POS = 2020UL

let main () =
    try
        let lines = File.ReadAllLines("input.txt")
        let mutable offset = 0UL
        let mutable increment = 1UL

        for line in lines do
            if line = "deal into new stack" then
                increment <- modMul increment (SIZE - 1UL) SIZE
                offset <- modAdd offset increment SIZE
            elif line.StartsWith("cut") then
                let nSigned = Int64.Parse(line.Substring(4).Trim())
                let n = if nSigned >= 0L then uint64 nSigned else SIZE - (uint64 (-nSigned))
                let n = n % SIZE
                offset <- modAdd offset (modMul n increment SIZE) SIZE
            elif line.StartsWith("deal with increment") then
                let n = UInt64.Parse(line.Substring(20).Trim())
                let inv = modInverse n SIZE
                increment <- modMul increment inv SIZE

        // Calculate the effect of applying the shuffle ITERATIONS times
        let finalIncrement = modPow increment ITERATIONS SIZE

        let term1 = modSub finalIncrement 1UL SIZE
        let term2Inv = modInverse (modSub increment 1UL SIZE) SIZE
        let finalOffset = modMul (modMul offset term1 SIZE) term2Inv SIZE

        // Find the value at TARGET_POS (2020)
        let targetValTimesIncr = modMul TARGET_POS finalIncrement SIZE
        let answer = modAdd targetValTimesIncr finalOffset SIZE

        printfn "%i" answer
    with
    | ex -> printfn "Error: %s" ex.Message

main ()
