
open System
open System.IO

type Bot =
    { mutable lowType: int
      mutable lowId: int
      mutable highType: int
      mutable highId: int
      mutable chips: int[]
      mutable chipCount: int }

[<EntryPoint>]
let main _ =
    let maxBod = 256
    let maxOut = 256
    let bots = Array.init maxBod (fun _ ->
        { lowType = 0; lowId = 0; highType = 0; highId = 0; chips = [|0;0|]; chipCount = 0 })
    let output = Array.zeroCreate<int> maxOut

    for line in File.ReadAllLines("input.txt") do
        if line.StartsWith("value") then
            let parts = line.Split(' ')
            let v = int parts.[1]
            let b = int parts.[parts.Length - 1]
            let bot = bots.[b]
            bot.chips.[bot.chipCount] <- v
            bot.chipCount <- bot.chipCount + 1
        else
            let parts = line.Split(' ')
            let b = int parts.[1]
            let bot = bots.[b]
            bot.lowType <- if parts.[5] = "output" then 1 else 0
            bot.lowId <- int parts.[6]
            bot.highType <- if parts.[10] = "output" then 1 else 0
            bot.highId <- int parts.[11]

    let mutable target = -1
    let mutable moved = true
    while moved do
        moved <- false
        for i = 0 to maxBod - 1 do
            let bot = bots.[i]
            if bot.chipCount = 2 then
                moved <- true
                let lo = min bot.chips.[0] bot.chips.[1]
                let hi = max bot.chips.[0] bot.chips.[1]
                if lo = 17 && hi = 61 then target <- i
                if bot.lowType = 0 then
                    let targetBot = bots.[bot.lowId]
                    targetBot.chips.[targetBot.chipCount] <- lo
                    targetBot.chipCount <- targetBot.chipCount + 1
                else
                    output.[bot.lowId] <- lo
                if bot.highType = 0 then
                    let targetBot = bots.[bot.highId]
                    targetBot.chips.[targetBot.chipCount] <- hi
                    targetBot.chipCount <- targetBot.chipCount + 1
                else
                    output.[bot.highId] <- hi
                bot.chipCount <- 0

    printfn "%d" target
    0
