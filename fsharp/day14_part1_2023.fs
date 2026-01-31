
open System.IO

let readInput () =
    File.ReadAllLines "input.txt"
    |> Array.map (fun s -> s.ToCharArray())

let tiltNorth (grid: char[][]) =
    let rows = grid.Length
    let cols = grid.[0].Length
    for x in 0..cols-1 do
        let mutable top = 0
        for y in 0..rows-1 do
            match grid.[y].[x] with
            | 'O' ->
                grid.[y].[x] <- '.'
                grid.[top].[x] <- 'O'
                top <- top + 1
            | '#' -> top <- y + 1
            | _ -> ()
    grid

let calculateLoad (grid: char[][]) =
    let rows = grid.Length
    [| for y in 0..rows-1 do
         for x in 0..grid.[y].Length-1 do
           if grid.[y].[x] = 'O' then rows - y else 0 |]
    |> Array.sum

[<EntryPoint>]
let main _ =
    let grid = readInput ()
    tiltNorth grid |> ignore
    calculateLoad grid |> printfn "%d"
    0
