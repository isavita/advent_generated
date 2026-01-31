
open System
open System.IO

type Hand = { cards: string; bid: int }

let cardValue c =
    match c with
    | c when Char.IsDigit c -> int c - int '0'
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> 0

let handType (cards: string) =
    let counts = Array.create 15 0
    for c in cards do
        let v = cardValue c
        counts.[v] <- counts.[v] + 1
    let mutable pairs, three, four, five = 0, 0, 0, 0
    for i in 2..14 do
        match counts.[i] with
        | 2 -> pairs <- pairs + 1
        | 3 -> three <- three + 1
        | 4 -> four <- four + 1
        | 5 -> five <- five + 1
        | _ -> ()
    if five = 1 then 7
    elif four = 1 then 6
    elif three = 1 && pairs = 1 then 5
    elif three = 1 then 4
    elif pairs = 2 then 3
    elif pairs = 1 then 2
    else 1

let calcRank (cards: string) =
    let mutable rank = 0L
    for c in cards do
        rank <- rank * 16L + int64 (cardValue c)
    rank

let hands =
    File.ReadAllLines "input.txt"
    |> Array.filter (fun s -> s.Trim().Length > 0)
    |> Array.map (fun line ->
        let parts = line.Split(' ')
        { cards = parts.[0]; bid = int parts.[1] })

let buckets = Array.init 7 (fun _ -> ResizeArray<Hand * int64>())

for h in hands do
    let t = handType h.cards - 1
    buckets.[t].Add(h, calcRank h.cards)

let all =
    buckets
    |> Array.rev
    |> Array.collect (fun l ->
        l
        |> Seq.sortByDescending snd
        |> Seq.map fst
        |> Array.ofSeq)

let total =
    all
    |> Array.mapi (fun i h -> int64 h.bid * (int64 all.Length - int64 i))
    |> Array.sum

printfn "%d" total
