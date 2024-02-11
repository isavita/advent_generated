
module Day22

let input = System.IO.File.ReadAllLines "input.txt"

let deckSize = 10007
let cardToFind = 2019

let dealIntoNewStack deck = List.rev deck

let cutN deck n =
    let n = if n >= 0 then n else deckSize + n
    let top, bottom = List.splitAt n deck
    bottom @ top

let dealWithIncrementN deck n =
    let result = Array.create deckSize (-1)
    let mutable pos = 0
    for i in deck do
        result.[pos] <- i
        pos <- (pos + n) % deckSize
    result |> Array.toList

let shuffleDeck deck =
    let mutable currentDeck = deck
    for line in input do
        match line.Split(' ') with
        | [| "deal"; "into"; "new"; "stack" |] -> currentDeck <- dealIntoNewStack currentDeck
        | [| "cut"; n |] -> currentDeck <- cutN currentDeck (int n)
        | [| "deal"; "with"; "increment"; n |] -> currentDeck <- dealWithIncrementN currentDeck (int n)
    currentDeck

let result = shuffleDeck [0..deckSize-1] |> List.findIndex (fun x -> x = cardToFind)

printfn "%A" result
