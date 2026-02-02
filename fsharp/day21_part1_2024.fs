
open System
open System.IO
open System.Text
open System.Collections.Generic

let findPos (m:string[]) (c:char) =
    let mutable r = (-1, -1)
    for i = 0 to m.Length - 1 do
        let j = m.[i].IndexOf(c)
        if j >= 0 then r <- (i, j)
    r

let ok (m:string[]) (si:int) (sj:int) (seq:string) =
    let mutable i = si
    let mutable j = sj
    let mutable ok = true
    for ch in seq do
        if not ok then ()
        elif m.[i].[j] = ' ' then ok <- false
        else
            i <- i + (if ch = 'v' then 1 elif ch = '^' then -1 else 0)
            j <- j + (if ch = '>' then 1 elif ch = '<' then -1 else 0)
            if i < 0 || i >= m.Length || j < 0 || j >= m.[0].Length then ok <- false
    ok

let genMoves (pos:int*int) (obj:char) (pad:string[]) =
    let (pi, pj) = pos
    let (oi, oj) = findPos pad obj
    let sb = StringBuilder()
    if pj > oj then sb.Append(String(' ', 0).Replace(" ", "<") |> fun _ -> new string('<', pj - oj)) |> ignore
    if pi > oi then sb.Append(new string('^', pi - oi)) |> ignore
    if pi < oi then sb.Append(new string('v', oi - pi)) |> ignore
    if pj < oj then sb.Append(new string('>', oj - pj)) |> ignore
    let cand = sb.ToString()
    if ok pad pi pj cand then cand
    else
        sb.Clear() |> ignore
        if pj < oj then sb.Append(new string('>', oj - pj)) |> ignore
        if pi > oi then sb.Append(new string('^', pi - oi)) |> ignore
        if pi < oi then sb.Append(new string('v', oi - pi)) |> ignore
        if pj > oj then sb.Append(new string('<', pj - oj)) |> ignore
        sb.ToString()

let rec solve (code:string) (robots:int) (keyPad:string[]) (robotPad:string[]) (maxRobots:int) (memo:Dictionary<string,int64>) =
    if robots <= 0 then int64 code.Length
    else
        let state = code + "," + string robots
        match memo.TryGetValue(state) with
        | true, v -> v
        | _ ->
            let mutable sum = 0L
            let mutable pi = if robots = maxRobots then 3 else 0
            let mutable pj = 2
            for ch in code do
                let moves, np =
                    if robots = maxRobots then
                        let mv = genMoves (pi, pj) ch keyPad
                        mv, findPos keyPad ch
                    else
                        let mv = genMoves (pi, pj) ch robotPad
                        mv, findPos robotPad ch
                pi <- fst np
                pj <- snd np
                sum <- sum + solve (moves + "A") (robots - 1) keyPad robotPad maxRobots memo
            memo.[state] <- sum
            sum

[<EntryPoint>]
let main _ =
    let maxRobots = 3
    let keyPad = [| "789"; "456"; "123"; " 0A" |]
    let robotPad = [| " ^A"; "<v>" |]
    let mutable total = 0L
    for line in File.ReadLines("input.txt") do
        let s = line.Trim()
        if s <> "" then
            let num =
                s |> Seq.filter Char.IsDigit
                  |> Seq.fold (fun acc d -> acc * 10L + int64 (int d - int '0')) 0L
            let memo = Dictionary<string,int64>()
            total <- total + solve s maxRobots keyPad robotPad maxRobots memo * num
    printfn "%d" total
    0
