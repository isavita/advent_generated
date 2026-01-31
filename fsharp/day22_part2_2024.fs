
open System
open System.IO

let [<Literal>] NUM_STEPS = 2000
let [<Literal>] MOD = 1 <<< 24
let [<Literal>] MOD_MASK = MOD - 1

let inline nextSecret (s: uint32) =
    let x = s * 64u
    let s = (s ^^^ x) &&& uint32 MOD_MASK
    let x = s / 32u
    let s = (s ^^^ x) &&& uint32 MOD_MASK
    let x = s * 2048u
    (s ^^^ x) &&& uint32 MOD_MASK

let inline encode4 c1 c2 c3 c4 =
    let c1, c2, c3, c4 = c1 + 9, c2 + 9, c3 + 9, c4 + 9
    c1 + c2 * 19 + c3 * 361 + c4 * 6859

let initials =
    File.ReadAllLines "input.txt"
    |> Array.choose (fun line -> match Int32.TryParse line with true, v -> Some v | _ -> None)

let PATTERN_COUNT = 130321
let globalSum = Array.zeroCreate<int64> PATTERN_COUNT
let prices = Array.zeroCreate<int> (NUM_STEPS + 1)
let changes = Array.zeroCreate<int> NUM_STEPS
let localPrice = Array.create PATTERN_COUNT -1

for init in initials do
    let mutable s = uint32 init
    for i in 0 .. NUM_STEPS do
        prices.[i] <- int s % 10
        s <- nextSecret s
    for j in 0 .. NUM_STEPS - 1 do
        changes.[j] <- prices.[j + 1] - prices.[j]

    Array.fill localPrice 0 PATTERN_COUNT -1

    for i in 0 .. NUM_STEPS - 4 do
        let c1, c2, c3, c4 = changes.[i], changes.[i+1], changes.[i+2], changes.[i+3]
        if abs c1 <= 9 && abs c2 <= 9 && abs c3 <= 9 && abs c4 <= 9 then
            let idx = encode4 c1 c2 c3 c4
            if localPrice.[idx] < 0 then
                localPrice.[idx] <- prices.[i + 4]

    for idx in 0 .. PATTERN_COUNT - 1 do
        if localPrice.[idx] >= 0 then
            globalSum.[idx] <- globalSum.[idx] + int64 localPrice.[idx]

printfn "%d" (Array.max globalSum)
