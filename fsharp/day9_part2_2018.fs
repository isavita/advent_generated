
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

[<EntryPoint>]
let main argv =
    let line = File.ReadAllText "input.txt"
    let m = Regex.Match(line, @"(\d+) players; last marble is worth (\d+) points")
    let players = int m.Groups.[1].Value
    let lastMarble = int m.Groups.[2].Value * 100
    let scores = Array.zeroCreate<int64> players
    let circle = LinkedList<int>()
    let first = circle.AddLast(0)
    let mutable cur = first
    for marble = 1 to lastMarble do
        if marble % 23 = 0 then
            let player = (marble - 1) % players
            for _ in 1 .. 7 do
                cur <- if cur.Previous <> null then cur.Previous else circle.Last
            scores.[player] <- scores.[player] + int64 marble + int64 cur.Value
            let toRemove = cur
            cur <- if toRemove.Next <> null then toRemove.Next else circle.First
            circle.Remove(toRemove)
        else
            cur <- if cur.Next <> null then cur.Next else circle.First
            let node = circle.AddAfter(cur, marble)
            cur <- node
    let maxScore = Array.max scores
    printfn "%d" maxScore
    0
