
open System
open System.IO

[<EntryPoint>]
let main _ =
    let maxSize = 30
    let cycles = 6
    let mutable grid = Array4D.create maxSize maxSize maxSize maxSize false
    let input = File.ReadAllLines "input.txt"
    let mid = maxSize / 2
    let offset = mid - input.Length / 2
    for i = 0 to input.Length - 1 do
        for j = 0 to input.[i].Length - 1 do
            grid.[mid,mid,offset + i,offset + j] <- input.[i].[j] = '#'
    let countActive (g:bool[,,,]) x y z w =
        let mutable c = 0
        for i = -1 to 1 do
            for j = -1 to 1 do
                for k = -1 to 1 do
                    for l = -1 to 1 do
                        if not (i = 0 && j = 0 && k = 0 && l = 0) then
                            let xi = x + i
                            let yj = y + j
                            let zk = z + k
                            let wl = w + l
                            if xi >= 0 && xi < maxSize &&
                               yj >= 0 && yj < maxSize &&
                               zk >= 0 && zk < maxSize &&
                               wl >= 0 && wl < maxSize &&
                               g.[xi,yj,zk,wl] then c <- c + 1
        c
    for _ = 0 to cycles - 1 do
        let newGrid = Array4D.create maxSize maxSize maxSize maxSize false
        for x = 1 to maxSize - 2 do
            for y = 1 to maxSize - 2 do
                for z = 1 to maxSize - 2 do
                    for w = 1 to maxSize - 2 do
                        let a = grid.[x,y,z,w]
                        let n = countActive grid x y z w
                        newGrid.[x,y,z,w] <- if a then n = 2 || n = 3 else n = 3
        grid <- newGrid
    let mutable total = 0
    for x = 0 to maxSize - 1 do
        for y = 0 to maxSize - 1 do
            for z = 0 to maxSize - 1 do
                for w = 0 to maxSize - 1 do
                    if grid.[x,y,z,w] then total <- total + 1
    printfn "%d" total
    0
