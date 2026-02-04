
open System
open System.IO
open System.Text.RegularExpressions

let appendChar (a:char[]) c = Array.append a [|c|]
let prependChar (a:char[]) c = Array.append [|c|] a
let appendRow (a:char[][]) row = Array.append a [|row|]

[<EntryPoint>]
let main _ =
    let lines = File.ReadLines("input.txt") |> Seq.map (fun s -> s.Trim()) |> Seq.toArray
    let mutable ground = [| [| '+' |] |]
    let mutable maxX = 0
    let mutable minX = 0
    let mutable maxY = 0
    let mutable minY = 20
    let xOffset = 500
    let yOffset = 0
    let pat = Regex(@"([xy])=(\d+), [xy]=(\d+)..(\d+)", RegexOptions.Compiled)

    for l in lines do
        let m = pat.Match(l)
        if m.Success then
            let typ = m.Groups.[1].Value
            let a = int m.Groups.[2].Value - (if typ = "x" then xOffset else yOffset)
            let b = int m.Groups.[3].Value - (if typ = "x" then yOffset else xOffset)
            let c = int m.Groups.[4].Value - (if typ = "x" then yOffset else xOffset)

            if typ = "x" then
                let x = a
                let y1 = b
                let y2 = c
                while x >= maxX do
                    maxX <- maxX + 1
                    for i = 0 to ground.Length-1 do
                        ground.[i] <- appendChar ground.[i] '.'
                while x <= minX do
                    minX <- minX - 1
                    for i = 0 to ground.Length-1 do
                        ground.[i] <- prependChar ground.[i] '.'
                while y2 > maxY do
                    maxY <- maxY + 1
                    ground <- appendRow ground (Array.create ground.[0].Length '.')
                minY <- min minY y1
                for i = y1 to y2 do
                    ground.[i].[x - minX] <- '#'
            else
                let y = a
                let x1 = b
                let x2 = c
                while y > maxY do
                    maxY <- maxY + 1
                    ground <- appendRow ground (Array.create ground.[0].Length '.')
                while x2 >= maxX do
                    maxX <- maxX + 1
                    for i = 0 to ground.Length-1 do
                        ground.[i] <- appendChar ground.[i] '.'
                while x1 <= minX do
                    minX <- minX - 1
                    for i = 0 to ground.Length-1 do
                        ground.[i] <- prependChar ground.[i] '.'
                for i = x1 to x2 do
                    ground.[y].[i - minX] <- '#'
                minY <- min minY y

    let mutable water = 0
    let mutable flow = 0
    let limit = 200000
    while ground.[1].[ -minX] <> '|' && water < limit do
        let mutable moving = true
        let mutable x = -minX
        let mutable y = 1
        let mutable tryLeft = 0
        while moving do
            if y + 1 > maxY || ground.[y+1].[x] = '|' then
                ground.[y].[x] <- '|'
                moving <- false
                if y >= minY then flow <- flow + 1
            elif ground.[y+1].[x] = '.' then
                y <- y + 1
                tryLeft <- 0
            else
                if (tryLeft = 1 && ground.[y].[x-1] = '|') ||
                   (tryLeft = 2 && ground.[y].[x+1] = '|') ||
                   (ground.[y].[x+1] = '|' && ground.[y].[x-1] <> '.') ||
                   (ground.[y].[x+1] <> '.' && ground.[y].[x-1] = '|') then
                    ground.[y].[x] <- '|'
                    flow <- flow + 1
                    moving <- false
                    let mutable i = x + 1
                    while i < ground.[0].Length && ground.[y].[i] = '~' do
                        ground.[y].[i] <- '|'
                        water <- water - 1
                        flow <- flow + 1
                        i <- i + 1
                    let mutable i = x - 1
                    while i >= 0 && ground.[y].[i] = '~' do
                        ground.[y].[i] <- '|'
                        water <- water - 1
                        flow <- flow + 1
                        i <- i - 1
                elif (tryLeft = 0 && ground.[y].[x-1] = '.') || (tryLeft = 1 && ground.[y].[x-1] = '.') then
                    x <- x - 1
                    tryLeft <- 1
                elif (tryLeft = 0 && ground.[y].[x+1] = '.') || (tryLeft = 2 && ground.[y].[x+1] = '.') then
                    x <- x + 1
                    tryLeft <- 2
                else
                    moving <- false
                    ground.[y].[x] <- '~'
                    water <- water + 1

    printfn "%d" water
    0
