
open System
open System.IO

type SnailNumber =
| Regular of int
| Pair of SnailNumber * SnailNumber

let rec explode (sn: SnailNumber) (depth: int) : (bool * SnailNumber * int option * int option) =
    match sn with
    | Regular _ -> (false, sn, None, None)
    | Pair (l, r) ->
        if depth = 4 then
            match l, r with
            | Regular lv, Regular rv -> (true, Regular 0, Some lv, Some rv)
            | _ -> failwith "unexpected non‑regular pair at depth 4"
        else
            let (ex1, l', lAdd, rAdd) = explode l (depth + 1)
            if ex1 then
                let r' = match rAdd with | Some v -> addLeft r v | None -> r
                (true, Pair (l', r'), lAdd, None)
            else
                let (ex2, r', lAdd2, rAdd2) = explode r (depth + 1)
                if ex2 then
                    let l' = match lAdd2 with | Some v -> addRight l v | None -> l
                    (true, Pair (l', r'), None, rAdd2)
                else
                    (false, sn, None, None)

and addLeft (sn: SnailNumber) (v: int) : SnailNumber =
    match sn with
    | Regular x -> Regular (x + v)
    | Pair (l, r) -> Pair (addLeft l v, r)

and addRight (sn: SnailNumber) (v: int) : SnailNumber =
    match sn with
    | Regular x -> Regular (x + v)
    | Pair (l, r) -> Pair (l, addRight r v)

let rec split (sn: SnailNumber) : (bool * SnailNumber) =
    match sn with
    | Regular v ->
        if v >= 10 then
            let l = v / 2
            let r = (v + 1) / 2
            (true, Pair (Regular l, Regular r))
        else
            (false, sn)
    | Pair (l, r) ->
        let (s1, l') = split l
        if s1 then (true, Pair (l', r))
        else
            let (s2, r') = split r
            (s2, Pair (l, r'))

let rec reduce (sn: SnailNumber) : SnailNumber =
    let rec loop current =
        let (ex, newSn, _, _) = explode current 0
        if ex then loop newSn
        else
            let (s, newSn2) = split current
            if s then loop newSn2
            else current
    loop sn

let add (a: SnailNumber) (b: SnailNumber) : SnailNumber =
    reduce (Pair (a, b))

let rec deepCopy (sn: SnailNumber) : SnailNumber =
    match sn with
    | Regular v -> Regular v
    | Pair (l, r) -> Pair (deepCopy l, deepCopy r)

let rec magnitude (sn: SnailNumber) : int =
    match sn with
    | Regular v -> v
    | Pair (l, r) -> 3 * magnitude l + 2 * magnitude r

let parse (s: string) : SnailNumber =
    let stack = System.Collections.Generic.Stack<SnailNumber>()
    for c in s do
        if Char.IsDigit(c) then
            stack.Push (Regular (int c - int '0'))
        elif c = ']' then
            let right = stack.Pop()
            let left = stack.Pop()
            stack.Push (Pair (left, right))
    stack.Pop()

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let snails = lines |> Array.map parse
    let n = snails.Length
    let mutable maxMag = 0
    for i in 0 .. n - 1 do
        for j in 0 .. n - 1 do
            if i <> j then
                let m = magnitude (add (deepCopy snails.[i]) (deepCopy snails.[j]))
                if m > maxMag then maxMag <- m
    printfn "%d" maxMag
    0
