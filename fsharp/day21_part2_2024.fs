
open System
open System.Collections.Generic
open System.IO

let repeat (c:char) n =
    if n > 0 then new string(c, n) else ""

let findPosition (mat:string[]) (ch:char) : int * int =
    let mutable res = (-1, -1)
    for i = 0 to mat.Length - 1 do
        let j = mat.[i].IndexOf(ch)
        if j <> -1 then res <- (i, j)
    res

let ok (mat:string[]) (stI:int) (stJ:int) (seq:string) =
    let mutable i = stI
    let mutable j = stJ
    let mutable ok = true
    for ch in seq do
        if not ok then ()
        elif i < 0 || i >= mat.Length || j < 0 || j >= mat.[i].Length || mat.[i].[j] = ' ' then
            ok <- false
        else
            match ch with
            | '^' -> i <- i - 1
            | 'v' -> i <- i + 1
            | '<' -> j <- j - 1
            | '>' -> j <- j + 1
            | _   -> ()
    ok

let generateMoves (posI:int, posJ:int) (objective:char) (pad:string[]) =
    let (objI,objJ) = findPosition pad objective
    // try straight order
    let mutable result =
        (if posJ > objJ then repeat '<' (posJ-objJ) else "")
        + (if posI > objI then repeat '^' (posI-objI) else "")
        + (if posI < objI then repeat 'v' (objI-posI) else "")
        + (if posJ < objJ then repeat '>' (objJ-posJ) else "")
    if ok pad posI posJ result then result
    else
        // alternative order
        (if posJ < objJ then repeat '>' (objJ-posJ) else "")
        + (if posI > objI then repeat '^' (posI-objI) else "")
        + (if posI < objI then repeat 'v' (objI-posI) else "")
        + (if posJ > objJ then repeat '<' (posJ-objJ) else "")

let rec solve
        (code:string) (robots:int) (keyPad:string[]) (robotPad:string[])
        (maxRobots:int) (memo:Dictionary<(string*int*int),int64>) : int64 =

    let key = (code, robots, maxRobots)
    match memo.TryGetValue(key) with
    | true, v -> v
    | false, _ ->
        if robots <= 0 then int64 code.Length
        else
            let mutable ret = 0L
            // initial cursor position
            let mutable posI = 3
            let mutable posJ = 2
            if robots <> maxRobots then posI <- 0   // posJ stays 2, same as the original C# logic

            for ch in code do
                let moves, newI, newJ =
                    if robots = maxRobots then
                        let mv = generateMoves (posI,posJ) ch keyPad
                        let (ni,nj) = findPosition keyPad ch
                        (mv, ni, nj)
                    else
                        let mv = generateMoves (posI,posJ) ch robotPad
                        let (ni,nj) = findPosition robotPad ch
                        (mv, ni, nj)

                posI <- newI
                posJ <- newJ
                ret <- ret + solve (moves + "A") (robots-1) keyPad robotPad maxRobots memo

            memo.[key] <- ret
            ret

[<EntryPoint>]
let main _ =
    let content = File.ReadAllLines("input.txt")
    let maxRobots = 26
    let keyPad   = [| "789"; "456"; "123"; " 0A" |]
    let robotPad = [| " ^A"; "<v>" |]

    let mutable total = 0L
    let memo = Dictionary<(string*int*int),int64>()

    for line in content do
        let code = line.Trim()
        if not (String.IsNullOrWhiteSpace code) then
            // numeric part of the code
            let mutable numericPart = 0L
            for c in code do
                if Char.IsDigit c then
                    numericPart <- numericPart * 10L + int64 (int c - int '0')
            let value = solve code maxRobots keyPad robotPad maxRobots memo
            total <- total + value * numericPart
            memo.Clear()   // fresh memo for the next code

    printfn "%d" total
    0
