
open System
open System.Collections.Generic
open System.IO

let main =
    let input = File.ReadAllText("input.txt").Trim()
    let startingNumbers = input.Split(',') |> Array.map int

    let lastSpoken = new Dictionary<int, int>()
    let mutable lastNumber = 0
    let mutable turn = 1

    while turn <= startingNumbers.Length do
        lastNumber <- startingNumbers.[turn - 1]
        lastSpoken.[lastNumber] <- turn
        turn <- turn + 1

    while turn <= 2020 do
        let nextNumber =
            match lastSpoken.TryGetValue(lastNumber) with
            | true, lastTurn when lastTurn <> turn - 1 -> turn - 1 - lastTurn
            | _ -> 0

        lastSpoken.[lastNumber] <- turn - 1
        lastNumber <- nextNumber
        turn <- turn + 1

    printfn "%d" lastNumber

main
