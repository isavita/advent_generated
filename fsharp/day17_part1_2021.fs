
open System
open System.IO
open System.Text.RegularExpressions

let isMovingAway (xPos, yPos) (xVel, yVel) (xMin, xMax, yMin, yMax) =
    (xPos < xMin && xVel < 0) || (xPos > xMax && xVel > 0) || (yPos < yMin && yVel < 0)

let simulate (xVel, yVel) (xMin, xMax, yMin, yMax) =
    let rec loop (xPos, yPos) (curXVel, curYVel) highestY =
        let newXPos = xPos + curXVel
        let newYPos = yPos + curYVel
        if newXPos >= xMin && newXPos <= xMax && newYPos >= yMin && newYPos <= yMax then
            Some highestY
        elif isMovingAway (newXPos, newYPos) (curXVel, curYVel) (xMin, xMax, yMin, yMax) then
            None
        else
            let newXVel = if curXVel > 0 then curXVel - 1 elif curXVel < 0 then curXVel + 1 else 0
            let newYVel = curYVel - 1
            let newHighestY = max highestY newYPos
            loop (newXPos, newYPos) (newXVel, newYVel) newHighestY

    loop (0, 0) (xVel, yVel) 0

let main () =
    try
        let line = File.ReadAllLines("input.txt").[0]
        let regex = Regex "target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)"
        let match' = regex.Match(line)
        if match'.Success then
            let xMin, xMax, yMin, yMax = 
                int match'.Groups.[1].Value, int match'.Groups.[2].Value, 
                int match'.Groups.[3].Value, int match'.Groups.[4].Value
            let maxY =
                seq { for xVel in -200 .. 200 do
                      for yVel in -200 .. 200 do
                      match simulate (xVel, yVel) (xMin, xMax, yMin, yMax) with
                      | Some y -> yield y
                      | None -> () }
                |> Seq.max
            printfn "%d" maxY
        else
            printfn "Invalid input format"
    with
    | :? FileNotFoundException -> printfn "File not found"

main ()
