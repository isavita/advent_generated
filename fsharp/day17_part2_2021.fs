
open System
open System.IO

let isMovingAway (xPos, yPos) (xVel, yVel) (xMin, xMax, yMin, _) =
    (xPos < xMin && xVel < 0) || (xPos > xMax && xVel > 0) || (yPos < yMin && yVel < 0)

let simulate (xVel, yVel) (xMin, xMax, yMin, yMax) =
    let mutable xPos, yPos = 0, 0
    let mutable curXVel, curYVel = xVel, yVel
    let mutable inTargetArea = false
    while not inTargetArea && not (isMovingAway (xPos, yPos) (curXVel, curYVel) (xMin, xMax, yMin, yMax)) do
        xPos <- xPos + curXVel
        yPos <- yPos + curYVel
        inTargetArea <- xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax
        curXVel <- if curXVel > 0 then curXVel - 1 else if curXVel < 0 then curXVel + 1 else 0
        curYVel <- curYVel - 1
    inTargetArea

let main() =
    let lines = File.ReadAllLines("input.txt")
    let parts = lines.[0].Split(", ")
    let xRange = parts.[0].Substring(15).Split("..") |> Array.map int
    let yRange = parts.[1].Substring(2).Split("..") |> Array.map int
    let xMin, xMax = xRange.[0], xRange.[1]
    let yMin, yMax = yRange.[0], yRange.[1]
    let targetArea = (xMin, xMax, yMin, yMax)

    let count =
        seq { for xVel in -200 .. xMax do
              for yVel in yMin .. 200 do
                  if simulate (xVel, yVel) targetArea then yield 1 }
        |> Seq.sum

    printfn "%d" count

main()
