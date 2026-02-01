
open System
open System.IO

[<EntryPoint>]
let main _ =
    let width, height, layerSize = 25, 6, 25*6
    let data = File.ReadAllText("input.txt").Trim()
    let img = Array.create layerSize '2'
    for i in 0..layerSize..data.Length-1 do
        for j in 0..layerSize-1 do
            if img.[j] = '2' then img.[j] <- data.[i+j]
    for y in 0..height-1 do
        for x in 0..width-1 do
            printf "%c" (if img.[y*width+x] = '0' then ' ' else '#')
        printfn ""
    0
