open System
open System.IO
open System.Collections.Generic

type Node =
    | Int of int
    | List of Node list

let rec cmp (a: Node) (b: Node) : int =
    match a, b with
    | Int x, Int y -> x.CompareTo(y)
    | Int x, List ys -> cmp (List [Int x]) (List ys)
    | List xs, Int y -> cmp (List xs) (List [Int y])
    | List xs, List ys ->
        let n = min xs.Length ys.Length
        let mutable i = 0
        let mutable res = 0
        while i < n && res = 0 do
            res <- cmp xs.[i] ys.[i]
            i <- i + 1
        if res <> 0 then res
        else xs.Length.CompareTo(ys.Length)

let parseNode (s: string) : Node =
    let i = ref 0
    let rec parse () : Node =
        while !i < s.Length && Char.IsWhiteSpace(s.[!i]) do i := !i + 1
        if s.[!i] = '[' then
            i := !i + 1
            let arr = ResizeArray<Node>()
            while s.[!i] <> ']' do
                arr.Add(parse())
                if !i < s.Length && s.[!i] = ',' then i := !i + 1
            i := !i + 1
            List (List.ofSeq arr)
        else
            let mutable num = 0
            while !i < s.Length && Char.IsDigit(s.[!i]) do
                num <- num * 10 + int(s.[!i] - '0')
                i := !i + 1
            Int num
    parse()

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines("input.txt")
    let mutable sum = 0
    let mutable pair = 1
    let len = lines.Length
    let mutable idx = 0
    while idx < len do
        let left = parseNode lines.[idx]
        let right = parseNode lines.[idx + 1]
        if cmp left right <= 0 then sum <- sum + pair
        pair <- pair + 1
        idx <- idx + 3
    printfn "%d" sum
    0