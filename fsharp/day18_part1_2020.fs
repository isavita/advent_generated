
open System
open System.IO
open System.Text.RegularExpressions

let rec evaluate (expr:string) =
    if expr.Contains("(") then
        let openIdx = expr.LastIndexOf('(')
        let closeIdx = expr.IndexOf(')', openIdx)
        let inner = expr.Substring(openIdx + 1, closeIdx - openIdx - 1)
        let innerVal = evaluateSimple inner
        let newExpr = expr.Substring(0, openIdx) + string innerVal + expr.Substring(closeIdx + 1)
        evaluate newExpr
    else evaluateSimple expr

and evaluateSimple (expr:string) =
    let parts = Regex.Split(expr.Trim(), @"\s+")
    let mutable acc = int64 parts.[0]
    let mutable i = 1
    while i < parts.Length do
        let op = parts.[i].[0]
        let v = int64 parts.[i + 1]
        match op with
        | '+' -> acc <- acc + v
        | '*' -> acc <- acc * v
        | _   -> ()
        i <- i + 2
    acc

[<EntryPoint>]
let main _ =
    if not (File.Exists "input.txt") then
        printfn "Error opening file"
        1
    else
        let total =
            File.ReadAllLines "input.txt"
            |> Array.fold (fun sum line ->
                if String.IsNullOrWhiteSpace line then sum
                else sum + evaluate (line.Trim())) 0L
        printfn "%d" total
        0
