
open System
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic

type Card = 
    { winnings: Map<string, int>; givens: Map<string, int>; mutable totalCount: int }

let getPointsForCard (card: Card) =
    card.givens 
    |> Map.fold (fun acc key value -> 
        match Map.tryFind key card.winnings with
        | Some v -> acc + value * v
        | None -> acc) 0

let lexLineIntoCard (line: string) =
    let parts = line.Split([|": "|], StringSplitOptions.None)
    let cardData = parts.[1].Split([|" | "|], StringSplitOptions.None)

    let regex = new Regex("[0-9]{1,2}")
    let winnings = 
        regex.Matches(cardData.[0]) 
        |> Seq.cast<Match> 
        |> Seq.map (fun m -> m.Value) 
        |> Seq.countBy id 
        |> Map.ofSeq

    let givens = 
        regex.Matches(cardData.[1]) 
        |> Seq.cast<Match> 
        |> Seq.map (fun m -> m.Value) 
        |> Seq.countBy id 
        |> Map.ofSeq

    { winnings = winnings; givens = givens; totalCount = 1 }

let main =
    try
        let input = File.ReadAllLines("input.txt")
        let cards = 
            input 
            |> Array.filter (fun line -> line.Length > 0) 
            |> Array.map lexLineIntoCard

        for i = 0 to cards.Length - 1 do
            let points = getPointsForCard cards.[i]
            for j = 1 to min points (cards.Length - i - 1) do
                cards.[i + j].totalCount <- cards.[i + j].totalCount + cards.[i].totalCount

        let totalCards = 
            cards 
            |> Array.sumBy (fun card -> card.totalCount)

        printfn "%d" totalCards
    with
    | :? FileNotFoundException -> printfn "File 'input.txt' not found."
    | ex -> printfn "An error occurred: %s" ex.Message

main
