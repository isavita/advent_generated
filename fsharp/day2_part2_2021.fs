module Day9

open System

let main =
    let lines = System.IO.File.ReadAllLines("input.txt")
    let mutable horizontalPosition = 0
    let mutable depth = 0
    let mutable aim = 0

    for line in lines do
        let command = line.Split(' ')
        let direction = command.[0]
        let units = Int32.Parse(command.[1])

        match direction with
        | "forward" -> 
            horizontalPosition <- horizontalPosition + units
            depth <- depth + aim * units
        | "down" -> aim <- aim + units
        | "up" -> aim <- aim - units

    let product = horizontalPosition * depth
    Console.WriteLine(product)