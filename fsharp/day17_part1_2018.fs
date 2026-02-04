
open System
open System.IO

let allocate rows cols = Array.init rows (fun _ -> Array.create cols '.')

let grow (g: char[][]) rows cols newRows newCols =
    let ng = allocate newRows newCols
    for i = 0 to rows-1 do
        Array.blit g.[i] 0 ng.[i] 0 cols
    ng, newRows, newCols

let shift (g: char[][]) cols shift =
    let newCols = cols + shift
    let ng = allocate g.Length newCols
    for i = 0 to g.Length-1 do
        for j = 0 to cols-1 do
            ng.[i].[j+shift] <- g.[i].[j]
    ng, newCols

let toInt (s:string) =
    let mutable v = 0
    let mutable sign = 1
    let mutable i = 0
    if s.[0] = '-' then sign <- -1; i <- 1
    while i < s.Length do
        v <- v*10 + int s.[i] - int '0'
        i <- i+1
    v*sign

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let mutable rows = 1
    let mutable cols = 1
    let mutable minX = 0
    let mutable maxX = 0
    let mutable minY = 20
    let mutable maxY = 0
    let xOff = 500
    let yOff = 0
    let mutable ground = allocate rows cols
    ground.[0].[0] <- '+'

    let parse (line:string) =
        let parts = line.Replace('=',' ').Replace(',',' ').Replace('.',' ')
                     .Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        if parts.[0] = "x" then
            let x = toInt parts.[1] - xOff
            let y1 = toInt parts.[3] - yOff
            let y2 = toInt parts.[4] - yOff
            while x >= maxX do
                maxX <- maxX+1
                let ng, nr, nc = grow ground rows cols rows (maxX-minX+1)
                ground <- ng; rows <- nr; cols <- nc
            while x <= minX do
                minX <- minX-1
                let ng, nc = shift ground cols 1
                ground <- ng; cols <- nc
            while y2 > maxY do
                maxY <- maxY+1
                let ng, nr, nc = grow ground rows cols (maxY+1) cols
                ground <- ng; rows <- nr; cols <- nc
            if y1 < minY then minY <- y1
            for y = y1 to y2 do
                ground.[y].[x-minX] <- '#'
        else
            let y = toInt parts.[1] - yOff
            let x1 = toInt parts.[3] - xOff
            let x2 = toInt parts.[4] - xOff
            while y > maxY do
                maxY <- maxY+1
                let ng, nr, nc = grow ground rows cols (maxY+1) cols
                ground <- ng; rows <- nr; cols <- nc
            while x2 >= maxX do
                maxX <- maxX+1
                let ng, nr, nc = grow ground rows cols rows (maxX-minX+1)
                ground <- ng; rows <- nr; cols <- nc
            while x1 <= minX do
                minX <- minX-1
                let ng, nc = shift ground cols 1
                ground <- ng; cols <- nc
            for x = x1 to x2 do
                ground.[y].[x-minX] <- '#'
            if y < minY then minY <- y

    for l in lines do parse l

    let mutable water = 0
    let mutable flow = 0
    let limit = 200000
    while ground.[1].[-minX] <> '|' && water < limit do
        let mutable x = -minX
        let mutable y = 1
        let mutable tryLeft = 0
        let mutable can = true
        while can do
            if y+1 > maxY || ground.[y+1].[x] = '|' then
                ground.[y].[x] <- '|'
                can <- false
                if y >= minY then flow <- flow+1
            elif ground.[y+1].[x] = '.' then
                y <- y+1
                tryLeft <- 0
            else
                let leftFlow = (tryLeft = 1 && ground.[y].[x-1] = '|')
                let rightFlow = (tryLeft = 2 && ground.[y].[x+1] = '|')
                let cond1 = (ground.[y].[x+1] = '|' && ground.[y].[x-1] <> '.')
                let cond2 = (ground.[y].[x+1] <> '.' && ground.[y].[x-1] = '|')
                if leftFlow || rightFlow || cond1 || cond2 then
                    ground.[y].[x] <- '|'
                    flow <- flow+1
                    can <- false
                    let mutable i = x+1
                    while ground.[y].[i] = '~' do
                        ground.[y].[i] <- '|'
                        water <- water-1
                        flow <- flow+1
                        i <- i+1
                    let mutable i = x-1
                    while ground.[y].[i] = '~' do
                        ground.[y].[i] <- '|'
                        water <- water-1
                        flow <- flow+1
                        i <- i-1
                elif (tryLeft = 0 && ground.[y].[x-1] = '.') ||
                     (tryLeft = 1 && ground.[y].[x-1] = '.') then
                    x <- x-1
                    tryLeft <- 1
                elif (tryLeft = 0 && ground.[y].[x+1] = '.') ||
                     (tryLeft = 2 && ground.[y].[x+1] = '.') then
                    x <- x+1
                    tryLeft <- 2
                else
                    can <- false
                    ground.[y].[x] <- '~'
                    water <- water+1
    printfn "%d" (flow+water)
    0
