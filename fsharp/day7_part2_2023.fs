open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Hand = { cards: string; bid: int }

let valueOf c =
    match c with
    | 'J' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' -> 10
    | 'Q' -> 11
    | 'K' -> 12
    | 'A' -> 13
    | _ -> 0

let transform (s:string) =
    s.Replace("A","E").Replace("T","A").Replace("J","1").Replace("Q","C").Replace("K","D")

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let lines = input.Split([|'\n'|], StringSplitOptions.None)
    let reCards = Regex(@"[\dAKQJT]+")
    let reBid = Regex(@" [\d]+")

    let hands = List<Hand>()
    for line in lines do
        let l = line.TrimEnd('\r')
        if l.Length = 0 then
            ()
        else
            let cards = reCards.Match(l).Value
            let bidStr = reBid.Match(l).Value.Substring(1)
            let bid = int bidStr
            hands.Add({ cards = cards; bid = bid })

    let matches = Array.init 7 (fun _ -> new List<Hand>())

    for hand in hands do
        let count = new Dictionary<char,int>()
        for c in hand.cards do
            if count.ContainsKey(c) then count.[c] <- count.[c] + 1
            else count.[c] <- 1

        if count.ContainsKey('J') && count.['J'] > 0 then
            let mutable highV = 0
            let mutable highKey = 'J'
            for kv in count do
                let y = kv.Key
                if y <> 'J' then
                    let v = kv.Value
                    if v > highV then
                        highV <- v
                        highKey <- y
                    elif v = highV && valueOf y > valueOf highKey then
                        highKey <- y
            if highKey <> 'J' then
                let jval = count.['J']
                if count.ContainsKey(highKey) then
                    count.[highKey] <- count.[highKey] + jval
                else
                    count.[highKey] <- jval
                count.Remove('J') |> ignore

        let mutable prod = 1
        for v in count.Values do
            prod <- prod * v

        match prod with
        | 1 -> matches.[6].Add(hand)
        | 2 -> matches.[5].Add(hand)
        | 3 -> matches.[3].Add(hand)
        | 4 ->
            if count.Count = 2 then matches.[1].Add(hand)
            else matches.[4].Add(hand)
        | 5 -> matches.[0].Add(hand)
        | 6 -> matches.[2].Add(hand)
        | _ -> ()

    let mutable convertedMatches = new List<int[]>()

    for lst in matches do
        let mutable temp = new List<int[]>()
        for h in lst do
            let y = transform h.cards
            let valInt = Convert.ToInt32(y, 16)
            temp.Add([| valInt; h.bid |])
        let sorted = (temp :> seq<int[]>)
                      |> Seq.toList
                      |> List.sortByDescending (fun arr -> arr.[0])
        for arr in sorted do convertedMatches.Add(arr)

    let countC = convertedMatches.Count
    let mutable total = 0
    for x in 0 .. countC - 1 do
        total <- total + convertedMatches.[x].[1] * (countC - x)

    printfn "%d" total
    0