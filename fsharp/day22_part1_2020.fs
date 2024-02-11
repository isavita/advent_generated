
module Day22

let input = System.IO.File.ReadAllLines "input.txt"

let rec playGame (player1, player2) =
    match player1, player2 with
    | [], _ -> player2
    | _, [] -> player1
    | p1::rest1, p2::rest2 ->
        if p1 > p2 then
            playGame (rest1 @ [p1; p2], rest2)
        else
            playGame (rest1, rest2 @ [p2; p1])

let calculateScore deck =
    deck
    |> List.rev
    |> List.mapi (fun i card -> card * (i + 1))
    |> List.sum

let player1, player2 =
    let splitIndex = input |> Seq.findIndex (fun x -> x = "")
    input.[1..splitIndex-1] |> Array.map int, input.[splitIndex+2..] |> Array.map int

let finalDecks = playGame (Array.toList player1, Array.toList player2)

let answer = calculateScore finalDecks
printfn "%d" answer
