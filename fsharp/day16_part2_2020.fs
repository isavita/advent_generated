
open System
open System.IO
open System.Collections.Generic

type Rule = {
    Name:string
    Low1:int; High1:int
    Low2:int; High2:int
    IsValid:int -> bool
}

let parseRule (line:string) =
    match line.IndexOf ':' with
    | -1 -> None
    | colon ->
        let name = line.Substring(0, colon)
        let ranges = line.Substring(colon + 1).Trim().Split([|" or "|], StringSplitOptions.None)
        if ranges.Length <> 2 then None else
        let r1 = ranges.[0].Split('-')
        let r2 = ranges.[1].Split('-')
        if r1.Length <> 2 || r2.Length <> 2 then None else
        match Int32.TryParse r1.[0], Int32.TryParse r1.[1],
              Int32.TryParse r2.[0], Int32.TryParse r2.[1] with
        | (true,l1),(true,h1),(true,l2),(true,h2) ->
            let valid v = (v >= l1 && v <= h1) || (v >= l2 && v <= h2)
            Some { Name=name; Low1=l1; High1=h1; Low2=l2; High2=h2; IsValid=valid }
        | _ -> None

let parseTicket (line:string) = line.Split(',') |> Array.map int

[<EntryPoint>]
let main _ =
    let mutable rules = List<Rule>()
    let mutable myTicket = [||]
    let mutable nearby = List<int[]>()
    let mutable section = 0
    for line in File.ReadAllLines "input.txt" do
        if String.IsNullOrWhiteSpace line then
            section <- section + 1
        else
            match section with
            | 0 -> parseRule line |> Option.iter rules.Add
            | 1 -> if not (line.StartsWith "your ticket:") then myTicket <- parseTicket line
            | 2 -> if not (line.StartsWith "nearby tickets:") then nearby.Add (parseTicket line)
            | _ -> ()
    let validTickets =
        nearby
        |> Seq.filter (fun t -> t |> Array.forall (fun v -> rules |> Seq.exists (fun r -> r.IsValid v)))
        |> Seq.toArray
    let fieldCount = rules.Count
    let possible = Array.init fieldCount (fun _ -> HashSet<int>(seq {0 .. fieldCount-1}))
    for ticket in validTickets do
        for idx = 0 to ticket.Length-1 do
            let v = ticket.[idx]
            for i = 0 to fieldCount-1 do
                if not (rules.[i].IsValid v) then possible.[i].Remove idx |> ignore
    let finalPos = Array.zeroCreate<int> fieldCount
    let mutable resolved = 0
    while resolved < fieldCount do
        for i = 0 to fieldCount-1 do
            if possible.[i].Count = 1 then
                let pos = Seq.head possible.[i]
                finalPos.[i] <- pos
                for j = 0 to fieldCount-1 do possible.[j].Remove pos |> ignore
                resolved <- resolved + 1
    let product =
        finalPos
        |> Array.mapi (fun i pos -> if rules.[i].Name.StartsWith "departure" then int64 myTicket.[pos] else 1L)
        |> Array.fold (*) 1L
    printfn "%d" product
    0
