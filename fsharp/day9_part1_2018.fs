open System
open System.IO

type Marble = { mutable Value: int; mutable Prev: Marble; mutable Next: Marble }

[<EntryPoint>]
let main argv =
    let parts = File.ReadAllLines("input.txt").[0].Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let players = int parts.[0]
    let lastMarble = int parts.[6]
    let scores = Array.zeroCreate<int> players
    let mutable current = { Value = 0; Prev = Unchecked.defaultof<_>; Next = Unchecked.defaultof<_> }
    current.Prev <- current
    current.Next <- current
    for marble in 1 .. lastMarble do
        if marble % 23 = 0 then
            let player = marble % players
            for _ in 1 .. 7 do current <- current.Prev
            scores.[player] <- scores.[player] + marble + current.Value
            current.Prev.Next <- current.Next
            current.Next.Prev <- current.Prev
            current <- current.Next
        else
            current <- current.Next
            let newMarble = { Value = marble; Prev = current; Next = current.Next }
            current.Next.Prev <- newMarble
            current.Next <- newMarble
            current <- newMarble
    printfn "%d" (scores |> Array.max)
    0