
open System
open System.IO
open System.Text.RegularExpressions

let boardSize = 5

type BoardState = 
    { 
        board: int[,]; 
        mutable picked: bool[,] 
    }

let createBoardState (board: int[,]) = 
    { 
        board = board; 
        picked = Array2D.init boardSize boardSize (fun _ _ -> false) 
    }

let pickNum (num: int) (bs: BoardState) = 
    for r in 0 .. boardSize - 1 do
        for c in 0 .. boardSize - 1 do
            if bs.board.[r, c] = num then
                bs.picked.[r, c] <- true

    let isWinning = 
        Array.exists (fun i -> 
            let isFullRow = Array.forall (fun j -> bs.picked.[i, j]) [|0 .. boardSize - 1|]
            let isFullCol = Array.forall (fun j -> bs.picked.[j, i]) [|0 .. boardSize - 1|]
            isFullRow || isFullCol
        ) [|0 .. boardSize - 1|]

    isWinning

let calculateScore (bs: BoardState) = 
    seq { 
        for r in 0 .. boardSize - 1 do
            for c in 0 .. boardSize - 1 do
                if not bs.picked.[r, c] then
                    yield bs.board.[r, c]
    } 
    |> Seq.sum

let solve (nums: int[]) (boards: BoardState[]) = 
    let mutable lastWinningScore = -1
    let mutable alreadyWon = Array.init (Array.length boards) (fun _ -> false)

    for num in nums do
        for i in 0 .. Array.length boards - 1 do
            if not alreadyWon.[i] then
                if pickNum num boards.[i] then
                    lastWinningScore <- calculateScore boards.[i] * num
                    alreadyWon.[i] <- true

    lastWinningScore

let main () = 
    try
        let lines = File.ReadAllLines "input.txt"
        let nums = lines.[0].Split ',' |> Array.map int

        let boards = 
            seq { 
                for i in 2 .. lines.Length - 1 do
                    if i % (boardSize + 1) = 2 then
                        let board = 
                            Array.init boardSize (fun j -> 
                                let row = lines.[i + j].Split ' ' |> Array.filter (fun x -> x <> "") |> Array.map int
                                Array.init boardSize (fun k -> row.[k])
                            ) 
                            |> array2D
                        yield createBoardState board
            } 
            |> Seq.toArray

        let result = solve nums boards
        printfn "%d" result
    with
    | ex -> printfn "An error occurred: %s" ex.Message

main ()
