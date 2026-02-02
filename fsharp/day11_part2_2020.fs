
open System
open System.IO

type Point = { x: int; y: int }

let directions = 
    [| { x = -1; y = -1 }; { x = 0; y = -1 }; { x = 1; y = -1 }
       { x = -1; y = 0 }; { x = 1; y = 0 }
       { x = -1; y = 1 }; { x = 0; y = 1 }; { x = 1; y = 1 } |]

let countVisibleOccupied (seatingArea: char[][]) row col =
    let countVisibleOccupiedInDir dir =
        let rec loop r c =
            if r < 0 || r >= Array.length seatingArea || c < 0 || c >= Array.length seatingArea.[0] then 0
            else
                match seatingArea.[r].[c] with
                | 'L' -> 0
                | '#' -> 1
                | _ -> loop (r + dir.y) (c + dir.x)
        loop (row + dir.y) (col + dir.x)
    Array.sumBy countVisibleOccupiedInDir directions

let simulateSeatingPartTwo (seatingArea: char[][]) =
    let rows = Array.length seatingArea
    let cols = Array.length seatingArea.[0]
    let newSeatingArea = Array.map Array.copy seatingArea
    let mutable stabilized = true
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            match seatingArea.[i].[j] with
            | 'L' when countVisibleOccupied seatingArea i j = 0 ->
                newSeatingArea.[i].[j] <- '#'
                stabilized <- false
            | '#' when countVisibleOccupied seatingArea i j >= 5 ->
                newSeatingArea.[i].[j] <- 'L'
                stabilized <- false
            | _ -> ()
    (newSeatingArea, stabilized)

let countOccupiedSeats (seatingArea: char[][]) =
    Array.sumBy (Array.sumBy (function '#' -> 1 | _ -> 0)) seatingArea

let main () =
    let lines = File.ReadAllLines "input.txt"
    let seatingArea = Array.map (fun (line: string) -> line.ToCharArray()) lines
    let rec loop seatingArea =
        let (newSeatingArea, stabilized) = simulateSeatingPartTwo seatingArea
        if stabilized then countOccupiedSeats newSeatingArea
        else loop newSeatingArea
    printfn "%d" (loop seatingArea)

main ()
