module Day12

open System
open System.IO

type Ship = { x:int; y:int; waypointX:int; waypointY:int }

let main =
    use file = File.OpenText "input.txt"
    let ship = { x=0; y=0; waypointX=10; waypointY=1 }

    let rec processInstruction (ship:Ship) (action:char) (value:int) =
        match action with
        | 'N' -> { ship with waypointY = ship.waypointY + value }
        | 'S' -> { ship with waypointY = ship.waypointY - value }
        | 'E' -> { ship with waypointX = ship.waypointX + value }
        | 'W' -> { ship with waypointX = ship.waypointX - value }
        | 'L' -> rotateWaypoint ship (-value)
        | 'R' -> rotateWaypoint ship value
        | 'F' -> { x = ship.x + ship.waypointX * value; y = ship.y + ship.waypointY * value; waypointX = ship.waypointX; waypointY = ship.waypointY }
        | _ -> ship

    and rotateWaypoint (ship:Ship) (degrees:int) =
        let newDegrees = (degrees + 360) % 360
        match newDegrees with
        | 90 | -270 -> { ship with waypointX = ship.waypointY; waypointY = -ship.waypointX }
        | 180 | -180 -> { ship with waypointX = -ship.waypointX; waypointY = -ship.waypointY }
        | 270 | -90 -> { ship with waypointX = -ship.waypointY; waypointY = ship.waypointX }
        | _ -> ship

    let rec readLines (ship:Ship) (reader:StreamReader) =
        let line = reader.ReadLine()
        if line = null then
            let manhattanDistance = abs ship.x + abs ship.y
            Console.WriteLine(manhattanDistance)
        else
            let action = line.[0]
            let value = Int32.Parse(line.[1..])
            let newShip = processInstruction ship action value
            readLines newShip reader

    readLines ship file