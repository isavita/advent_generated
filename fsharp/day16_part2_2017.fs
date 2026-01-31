
open System
open System.IO

let spin (arr:char[]) x =
    let n = arr.Length
    let r = x % n
    if r <> 0 then
        Array.Reverse(arr)
        Array.Reverse(arr, 0, r)
        Array.Reverse(arr, r, n - r)

let exchange (arr:char[]) a b =
    let t = arr.[a]
    arr.[a] <- arr.[b]
    arr.[b] <- t

let partner (arr:char[]) a b =
    let mutable ia = -1
    let mutable ib = -1
    for i in 0 .. arr.Length - 1 do
        if arr.[i] = a then ia <- i
        if arr.[i] = b then ib <- i
    if ia <> -1 && ib <> -1 then exchange arr ia ib

type Move =
    | Spin of int
    | Exchange of int * int
    | Partner of char * char

let parse (s:string) =
    match s.[0] with
    | 's' -> Spin (int s.[1..])
    | 'x' ->
        let p = s.[1..].Split('/')
        Exchange (int p.[0], int p.[1])
    | 'p' ->
        let p = s.[1..].Split('/')
        Partner (p.[0].[0], p.[1].[0])
    | _ -> failwith "invalid move"

[<EntryPoint>]
let main _ =
    let line = File.ReadAllText("input.txt").Trim()
    let moves = line.Split(',') |> Array.map parse
    let programs = "abcdefghijklmnop".ToCharArray()
    let initial = String(programs)

    let mutable cycle = 0
    let mutable i = 0
    let mutable found = false
    while i < 1_000_000_000 && not found do
        for m in moves do
            match m with
            | Spin x -> spin programs x
            | Exchange (a,b) -> exchange programs a b
            | Partner (a,b) -> partner programs a b
        if String(programs) = initial then
            cycle <- i + 1
            found <- true
        i <- i + 1

    let remaining =
        if cycle = 0 then 1_000_000_000
        else 1_000_000_000 % cycle

    for _ in 1 .. remaining do
        for m in moves do
            match m with
            | Spin x -> spin programs x
            | Exchange (a,b) -> exchange programs a b
            | Partner (a,b) -> partner programs a b

    printfn "%s" (String(programs))
    0
