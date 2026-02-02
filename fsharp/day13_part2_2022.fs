
open System
open System.IO
open System.Text.RegularExpressions

// Data Structure
type Item =
    | Int of int
    | List of Item list

// Helper function to parse packet
let rec parsePacket (s: string) : Item * string =
    let s = s.Trim()
    if s.StartsWith "[" then
        let s = s.Substring(1)
        let mutable items = []
        let mutable s = s
        while s <> "" && s.[0] <> ']' do
            let item, rest = parsePacket s
            items <- item :: items
            s <- rest.Trim()
            if s <> "" && s.[0] = ',' then
                s <- s.Substring(1).Trim()
        if s = "" then
            failwith "Unbalanced brackets"
        items <- List.rev items
        s <- s.Substring(1)
        List items, s
    else
        let matchResult = Regex.Match(s, "^(\d+)")
        if matchResult.Success then
            let value = int matchResult.Groups.[1].Value
            Int value, s.Substring(matchResult.Length).Trim()
        else
            failwith "Invalid packet"

// Compare items
let rec compareItems (a: Item) (b: Item) : int =
    match a, b with
    | Int x, Int y -> compare x y
    | List xs, List ys ->
        let rec compareLists xs ys =
            match xs, ys with
            | [], [] -> 0
            | [], _ -> -1
            | _, [] -> 1
            | x :: xs, y :: ys ->
                let cmp = compareItems x y
                if cmp = 0 then compareLists xs ys else cmp
        compareLists xs ys
    | Int x, List ys -> compareItems (List [ Int x ]) (List ys)
    | List xs, Int y -> compareItems (List xs) (List [ Int y ])

// Read packets from file
let readPackets (filePath: string) : Item list =
    File.ReadAllLines(filePath)
    |> Seq.filter (fun line -> line.Trim() <> "")
    |> Seq.map (fun line -> fst (parsePacket line))
    |> List.ofSeq

// Main function
[<EntryPoint>]
let main argv =
    let packets = readPackets "input.txt"
    let divider1 = List [ List [ Int 2 ] ]
    let divider2 = List [ List [ Int 6 ] ]
    let packets = List.concat [ packets; [ divider1; divider2 ] ]
    let sortedPackets = List.sortWith compareItems packets
    let divider1Pos = (List.findIndex ((=) divider1) sortedPackets) + 1
    let divider2Pos = (List.findIndex ((=) divider2) sortedPackets) + 1
    printfn "%d" (divider1Pos * divider2Pos)
    0
