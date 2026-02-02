
open System
open System.IO

type BingoBoard = 
    { numbers: int[,]
      mutable marked: bool[,] }

let createBingoBoard() = 
    { numbers = Array2D.init 5 5 (fun _ _ -> 0)
      marked = Array2D.init 5 5 (fun _ _ -> false) }

let mark number (board: BingoBoard) =
    for i = 0 to 4 do
        for j = 0 to 4 do
            if board.numbers.[i, j] = number then
                board.marked.[i, j] <- true

let hasWon (board: BingoBoard) =
    let checkRow row =
        Array.init 5 (fun i -> board.marked.[row, i]) |> Array.forall id

    let checkCol col =
        Array.init 5 (fun i -> board.marked.[i, col]) |> Array.forall id

    Array.init 5 checkRow |> Array.exists id || Array.init 5 checkCol |> Array.exists id

let unmarkedSum (board: BingoBoard) =
    seq { for i = 0 to 4 do
          for j = 0 to 4 do
              if not board.marked.[i, j] then yield board.numbers.[i, j] }
    |> Seq.sum

let readFromLine (lines: string[]) startIndex =
    let board = createBingoBoard()
    for i = 0 to 4 do
        let parts = lines.[startIndex + i].Trim().Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        for j = 0 to 4 do
            board.numbers.[i, j] <- int parts.[j]
    board

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines("input.txt")
    let numbers = lines.[0].Split(',') |> Array.map int
    let boards =
        [| for i = 0 to (lines.Length - 2) / 6 - 1 do
               yield readFromLine lines (2 + i * 6) |]

    let mutable found = false
    for number in numbers do
        for board in boards do
            mark number board
            if not found && hasWon board then
                printfn "%d" (unmarkedSum board * number)
                found <- true
    0
