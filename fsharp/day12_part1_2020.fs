
module Day12

open System.IO
open System

type Ship = { x: int; y: int; facing: int }

let processInstruction (ship: Ship) (action: char) (value: int) =
    match action with
    | 'N' -> { ship with y = ship.y + value }
    | 'S' -> { ship with y = ship.y - value }
    | 'E' -> { ship with x = ship.x + value }
    | 'W' -> { ship with x = ship.x - value }
    | 'L' -> { ship with facing = (ship.facing - value + 360) % 360 }
    | 'R' -> { ship with facing = (ship.facing + value) % 360 }
    | 'F' -> 
        match ship.facing with
        | 0 -> { ship with x = ship.x + value }
        | 90 -> { ship with y = ship.y - value }
        | 180 -> { ship with x = ship.x - value }
        | 270 -> { ship with y = ship.y + value }
        | _ -> ship

let abs (x: int) =
    if x < 0 then -x else x

let mutable ship = { x = 0; y = 0; facing = 0 }

let file = File.ReadAllLines "input.txt"
for line in file do
    let action = line.[0]
    let value = int line.[1..]
    ship <- processInstruction ship action value

let manhattanDistance = abs ship.x + abs ship.y
printfn "%d" manhattanDistance
