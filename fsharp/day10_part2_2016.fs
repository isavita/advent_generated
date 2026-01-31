
open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let maxBots = 256
    let maxOutputs = 256
    let lowTo = Array.zeroCreate<int> maxBots
    let highTo = Array.zeroCreate<int> maxBots
    let chips = Array.init maxBots (fun _ -> Array.zeroCreate<int> 2)
    let chipCnt = Array.zeroCreate<int> maxBots
    let outputs = Array.zeroCreate<int> maxOutputs

    let give (target:int) (isBot:bool) (value:int) =
        if isBot then
            let c = chipCnt.[target]
            if c < 2 then
                chips.[target].[c] <- value
                chipCnt.[target] <- c + 1
        else
            outputs.[target] <- value

    let lines = File.ReadAllLines "input.txt"
    let reVal = Regex(@"value (\d+) goes to bot (\d+)")
    let reBot = Regex(@"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)")

    for line in lines do
        if reVal.IsMatch line then
            let m = reVal.Match line
            let v = int m.Groups.[1].Value
            let b = int m.Groups.[2].Value
            give b true v
        elif reBot.IsMatch line then
            let m = reBot.Match line
            let b = int m.Groups.[1].Value
            let lt = m.Groups.[2].Value
            let lId = int m.Groups.[3].Value
            let ht = m.Groups.[4].Value
            let hId = int m.Groups.[5].Value
            lowTo.[b]  <- if lt = "bot" then lId else -1 - lId
            highTo.[b] <- if ht = "bot" then hId else -1 - hId

    let mutable changed = true
    while changed do
        changed <- false
        for i = 0 to maxBots-1 do
            if chipCnt.[i] = 2 then
                changed <- true
                let a = chips.[i].[0]
                let b = chips.[i].[1]
                let low = if a < b then a else b
                let high = if a > b then a else b
                let lt = lowTo.[i]
                let ht = highTo.[i]
                give (if lt < 0 then -1 - lt else lt) (lt >= 0) low
                give (if ht < 0 then -1 - ht else ht) (ht >= 0) high
                chipCnt.[i] <- 0

    let result = outputs.[0] * outputs.[1] * outputs.[2]
    printfn "%d" result
    0
