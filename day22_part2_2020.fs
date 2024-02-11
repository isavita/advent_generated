
module Day22

let input = System.IO.File.ReadAllLines "input.txt"

let rec playGame (player1: int list) (player2: int list) =
    let rec playGame' (player1: int list) (player2: int list) (previousRounds: Set<(int list * int list)>) =
        match player1, player2 with
        | [], _ -> 2, player2
        | _, [] -> 1, player1
        | _, _ when Set.contains (player1, player2) previousRounds -> 1, player1
        | p1 :: rest1, p2 :: rest2 ->
            if List.length rest1 >= p1 && List.length rest2 >= p2 then
                let winner, _ = playGame' (List.take p1 rest1) (List.take p2 rest2) Set.empty
                match winner with
                | 1 -> playGame' (rest1 @ [p1; p2]) rest2 (Set.add (player1, player2) previousRounds)
                | _ -> playGame' rest1 (rest2 @ [p2; p1]) (Set.add (player1, player2) previousRounds)
            else
                if p1 > p2 then
                    playGame' (rest1 @ [p1; p2]) rest2 (Set.add (player1, player2) previousRounds)
                else
                    playGame' rest1 (rest2 @ [p2; p1]) (Set.add (player1, player2) previousRounds)
    playGame' player1 player2 Set.empty

let parseInput (input: string array) =
    let splitIndex = input |> Seq.findIndex (fun x -> x = "")
    let player1 = input.[1..splitIndex-1] |> Array.map int |> Array.toList
    let player2 = input.[splitIndex+2..] |> Array.map int |> Array.toList
    player1, player2

let player1, player2 = parseInput input
let _, winningDeck = playGame player1 player2

let score = 
    List.rev winningDeck
    |> List.mapi (fun i x -> (i + 1) * x)
    |> List.sum

printfn "%d" score
