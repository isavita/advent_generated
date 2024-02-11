
module Day11

open System.IO

let rec readLines (reader:StreamReader) acc =
    if reader.EndOfStream then
        acc
    else
        let line = reader.ReadLine()
        readLines reader (line::acc)

let seatingArea = 
    use reader = File.OpenText "input.txt"
    readLines reader []
    |> List.map (fun line -> line.ToCharArray())
    |> List.toArray

let rec simulateSeating (seatingArea:char[][]) =
    let rows = Array.length seatingArea
    let cols = Array.length seatingArea.[0]
    let newSeatingArea = Array.zeroCreate<char[]> rows
    for i in 0..rows-1 do
        newSeatingArea.[i] <- Array.copy seatingArea.[i]
    
    let mutable stabilized = true

    for i in 0..rows-1 do
        for j in 0..cols-1 do
            match seatingArea.[i].[j] with
            | 'L' ->
                if countAdjacentOccupied seatingArea i j = 0 then
                    newSeatingArea.[i].[j] <- '#'
                    stabilized <- false
            | '#' ->
                if countAdjacentOccupied seatingArea i j >= 4 then
                    newSeatingArea.[i].[j] <- 'L'
                    stabilized <- false
            | _ -> ()

    if stabilized then
        newSeatingArea
    else
        simulateSeating newSeatingArea

and countAdjacentOccupied (seatingArea:char[][]) row col =
    let mutable count = 0
    let rows = Array.length seatingArea
    let cols = Array.length seatingArea.[0]

    for i in row-1..row+1 do
        for j in col-1..col+1 do
            if i = row && j = col then
                ()
            elif i >= 0 && i < rows && j >= 0 && j < cols then
                if seatingArea.[i].[j] = '#' then
                    count <- count + 1

    count

let countOccupiedSeats (seatingArea:char[][]) =
    seatingArea
    |> Array.fold (fun acc row ->
        row
        |> Array.fold (fun count seat ->
            if seat = '#' then count + 1 else count
        ) acc
    ) 0

let finalSeatingArea = simulateSeating seatingArea
let occupiedSeatsCount = countOccupiedSeats finalSeatingArea
printfn "%d" occupiedSeatsCount
