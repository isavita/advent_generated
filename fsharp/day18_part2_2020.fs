
open System
open System.IO
open System.Text.RegularExpressions

type Token =
    | Number of int64
    | Plus
    | Times
    | LParen
    | RParen

let tokenize (s:string) =
    let re = Regex(@"(\d+|[+\*\(\)])")
    re.Matches(s)
    |> Seq.cast<Match>
    |> Seq.map (fun m ->
        match m.Value with
        | "+"  -> Plus
        | "*"  -> Times
        | "("  -> LParen
        | ")"  -> RParen
        | n   -> Number (Int64.Parse n))
    |> List.ofSeq

let rec factor1 tokens =
    match tokens with
    | Number n::rest -> (n, rest)
    | LParen::rest ->
        let (v, rest') = expr1 rest
        match rest' with
        | RParen::rest'' -> (v, rest'')
        | _ -> failwith "missing )"
    | _ -> failwith "factor1 error"

and expr1 tokens =
    let rec loop acc tokens =
        match tokens with
        | Plus::rest ->
            let (n, rest') = factor1 rest
            loop (acc + n) rest'
        | Times::rest ->
            let (n, rest') = factor1 rest
            loop (acc * n) rest'
        | [] | RParen::_ -> (acc, tokens)
        | _ -> failwith "unexpected token"
    let (n, rest) = factor1 tokens
    loop n rest

let rec factor2 tokens =
    match tokens with
    | Number n::rest -> (n, rest)
    | LParen::rest ->
        let (v, rest') = expr2 rest
        match rest' with
        | RParen::rest'' -> (v, rest'')
        | _ -> failwith "missing )"
    | _ -> failwith "factor2 error"

and term2 tokens =
    let rec loop acc tokens =
        match tokens with
        | Plus::rest ->
            let (n, rest') = factor2 rest
            loop (acc + n) rest'
        | _ -> (acc, tokens)
    let (n, rest) = factor2 tokens
    loop n rest

and expr2 tokens =
    let rec loop acc tokens =
        match tokens with
        | Times::rest ->
            let (n, rest') = term2 rest
            loop (acc * n) rest'
        | _ -> (acc, tokens)
    let (v, rest) = term2 tokens
    loop v rest

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines "input.txt"
    let mutable part1 = 0L
    let mutable part2 = 0L
    for line in lines do
        let tokens = tokenize line
        let (v1, _) = expr1 tokens
        let (v2, _) = expr2 tokens
        part1 <- part1 + v1
        part2 <- part2 + v2
    printfn "%d" part1
    printfn "%d" part2
    0
