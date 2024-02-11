
module Day3

let input = System.IO.File.ReadAllText("input.txt")

let parseInput (input:string) =
    input.Trim()

let calculateSteps (input:int) =
    let rec findRing n =
        let ringMax = n * n
        if input <= ringMax then n
        else findRing (n + 2)
    
    let ring = findRing 1
    let ringMax = ring * ring
    let sideLength = ring - 1
    let offset = (ring - 1) / 2
    let positionInRing = ringMax - input
    let positionInSide = positionInRing % sideLength
    let stepsToCenter = abs(positionInSide - offset)
    
    offset + stepsToCenter

let result = input |> parseInput |> int |> calculateSteps
printfn "%d" result
