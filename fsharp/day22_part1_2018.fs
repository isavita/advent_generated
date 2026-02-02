
open System
open System.IO

let readInput (filePath: string) =
    let lines = File.ReadAllLines(filePath)
    let depth = Int32.Parse(lines.[0].Substring(7))
    let target = lines.[1].Substring(8).Split(',')
    let targetX = Int32.Parse(target.[0])
    let targetY = Int32.Parse(target.[1])
    (depth, targetX, targetY)

let calculateRiskLevel (depth: int) (targetX: int) (targetY: int) =
    let cave = Array2D.zeroCreate (targetX + 1) (targetY + 1)
    let mutable riskLevel = 0
    for x in 0 .. targetX do
        for y in 0 .. targetY do
            let geologicIndex =
                if (x = 0 && y = 0) || (x = targetX && y = targetY) then 0
                elif y = 0 then x * 16807
                elif x = 0 then y * 48271
                else cave.[x-1, y] * cave.[x, y-1]
            cave.[x, y] <- (geologicIndex + depth) % 20183
            riskLevel <- riskLevel + cave.[x, y] % 3
    riskLevel

[<EntryPoint>]
let main argv =
    try
        let (depth, targetX, targetY) = readInput "input.txt"
        let riskLevel = calculateRiskLevel depth targetX targetY
        printfn "%d" riskLevel
        0
    with
    | ex -> printfn "Error: %s" ex.Message; 1
