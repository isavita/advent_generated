
open System
open System.IO

let totalCups = 1000000
let totalMoves = 10000000

let main =
    let input = File.ReadAllText("input.txt")
    let cups = Array.init (totalCups + 1) (fun _ -> 0)

    let mutable lastCup = 0
    for c in input do
        let cup = int c - int '0'
        if lastCup > 0 then
            cups.[lastCup] <- cup
        lastCup <- cup

    for i = input.Length + 1 to totalCups do
        cups.[lastCup] <- i
        lastCup <- i
    cups.[lastCup] <- int input.[0] - int '0'

    let mutable currentCup = int input.[0] - int '0'
    for _ = 0 to totalMoves - 1 do
        let pickup1 = cups.[currentCup]
        let pickup2 = cups.[pickup1]
        let pickup3 = cups.[pickup2]

        cups.[currentCup] <- cups.[pickup3]

        let mutable destinationCup = currentCup - 1
        if destinationCup = 0 then
            destinationCup <- totalCups
        while (destinationCup = pickup1 || destinationCup = pickup2 || destinationCup = pickup3) do
            destinationCup <- destinationCup - 1
            if destinationCup = 0 then
                destinationCup <- totalCups

        let temp = cups.[destinationCup]
        cups.[destinationCup] <- pickup1
        cups.[pickup3] <- temp

        currentCup <- cups.[currentCup]

    let cup1 = cups.[1]
    let cup2 = cups.[cup1]
    printfn "%d" ((int64 cup1) * (int64 cup2))

main
