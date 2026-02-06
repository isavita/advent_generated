
open System
open System.Collections.Generic
open System.IO
open System.Text

type Computer(program: int64[]) =
    let mem = Dictionary<int64, int64>()
    do program |> Array.iteri (fun i v -> mem.[int64 i] <- v)
    let mutable ip, rb = 0L, 0L
    let input, output = Queue<int64>(), Queue<int64>()
    member _.Input = input
    member _.Output = output
    member _.Run() =
        let mutable halted, waiting = false, false
        while not halted && not waiting do
            let instr = if mem.ContainsKey ip then mem.[ip] else 0L
            let op, m = int (instr % 100L), [| int (instr / 100L % 10L); int (instr / 1000L % 10L); int (instr / 10000L % 10L) |]
            let g i =
                let v = if mem.ContainsKey (ip + int64 i) then mem.[ip + int64 i] else 0L
                match m.[i-1] with 0 -> (if mem.ContainsKey v then mem.[v] else 0L) | 1 -> v | 2 -> (if mem.ContainsKey (rb + v) then mem.[rb + v] else 0L) | _ -> 0L
            let s i v =
                let a = if mem.ContainsKey (ip + int64 i) then mem.[ip + int64 i] else 0L
                let addr = match m.[i-1] with 0 -> a | 2 -> rb + a | _ -> -1L
                if addr <> -1L then mem.[addr] <- v
            match op with
            | 1 -> s 3 (g 1 + g 2); ip <- ip + 4L
            | 2 -> s 3 (g 1 * g 2); ip <- ip + 4L
            | 3 -> if input.Count = 0 then waiting <- true else s 1 (input.Dequeue()); ip <- ip + 2L
            | 4 -> output.Enqueue(g 1); ip <- ip + 2L
            | 5 -> if g 1 <> 0L then ip <- g 2 else ip <- ip + 3L
            | 6 -> if g 1 = 0L then ip <- g 2 else ip <- ip + 3L
            | 7 -> s 3 (if g 1 < g 2 then 1L else 0L); ip <- ip + 4L
            | 8 -> s 3 (if g 1 = g 2 then 1L else 0L); ip <- ip + 4L
            | 9 -> rb <- rb + g 1; ip <- ip + 2L
            | 99 -> halted <- true
            | _ -> halted <- true
        halted

[<EntryPoint>]
let main _ =
    let prog = File.ReadAllText("input.txt").Trim().Split(',') |> Array.map int64
    let comp = Computer(prog)
    comp.Run() |> ignore
    let sb = StringBuilder()
    while comp.Output.Count > 0 do sb.Append(char (comp.Output.Dequeue())) |> ignore
    let map = sb.ToString().Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    let h = map.Length
    let isS r c = r >= 0 && r < h && c >= 0 && c < map.[r].Length && map.[r].[c] <> '.'
    let mutable p1 = 0
    for r in 1 .. h - 2 do
        for c in 1 .. map.[r].Length - 2 do
            if map.[r].[c] = '#' && isS (r-1) c && isS (r+1) c && isS r (c-1) && isS r (c+1) then p1 <- p1 + r * c
    printfn "Part One: %d" p1

    let mutable rR, rC, rD = 0, 0, ' '
    for r in 0 .. h - 1 do
        for c in 0 .. map.[r].Length - 1 do
            if "^v<>" |> Seq.contains map.[r].[c] then rR <- r; rC <- c; rD <- map.[r].[c]
    
    let move d r c = match d with '^' -> r-1, c | 'v' -> r+1, c | '<' -> r, c-1 | '>' -> r, c+1 | _ -> r, c
    let turnL d = match d with '^' -> '<' | '<' -> 'v' | 'v' -> '>' | '>' -> '^' | _ -> ' '
    let turnR d = match d with '^' -> '>' | '>' -> 'v' | 'v' -> '<' | '<' -> '^' | _ -> ' '

    let path = ResizeArray<string>()
    let mutable curR, curC, curD, steps, moving = rR, rC, rD, 0, true
    while moving do
        let nR, nC = move curD curR curC
        if isS nR nC then steps <- steps + 1; curR <- nR; curC <- nC
        else
            if steps > 0 then path.Add(string steps); steps <- 0
            let lD = turnL curD
            if isS (fst (move lD curR curC)) (snd (move lD curR curC)) then path.Add("L"); curD <- lD
            else
                let rD = turnR curD
                if isS (fst (move rD curR curC)) (snd (move rD curR curC)) then path.Add("R"); curD <- rD
                else moving <- false
    if steps > 0 then path.Add(string steps)
    
    let tokens = path.ToArray()
    let mutable sol = ("", "", "", "")
    let mutable found = false
    for aL in 1 .. 10 do
        if not found then
            let A = tokens.[0 .. aL - 1]
            let AS = String.concat "," A
            if AS.Length <= 20 then
                let mutable bS = aL
                while bS + aL <= tokens.Length && tokens.[bS .. bS + aL - 1] = A do bS <- bS + aL
                for bL in 1 .. 10 do
                    if not found && bS + bL <= tokens.Length then
                        let B = tokens.[bS .. bS + bL - 1]
                        let BS = String.concat "," B
                        if BS.Length <= 20 then
                            let mutable cS, matchFound = bS, true
                            while matchFound do
                                if cS + aL <= tokens.Length && tokens.[cS .. cS + aL - 1] = A then cS <- cS + aL
                                elif cS + bL <= tokens.Length && tokens.[cS .. cS + bL - 1] = B then cS <- cS + bL
                                else matchFound <- false
                            for cL in 1 .. 10 do
                                if not found && cS + cL <= tokens.Length then
                                    let C = tokens.[cS .. cS + cL - 1]
                                    let CS = String.concat "," C
                                    if CS.Length <= 20 then
                                        let mutable p, ok, main = 0, true, ResizeArray<string>()
                                        while p < tokens.Length && ok do
                                            if p + aL <= tokens.Length && tokens.[p .. p + aL - 1] = A then main.Add("A"); p <- p + aL
                                            elif p + bL <= tokens.Length && tokens.[p .. p + bL - 1] = B then main.Add("B"); p <- p + bL
                                            elif p + cL <= tokens.Length && tokens.[p .. p + cL - 1] = C then main.Add("C"); p <- p + cL
                                            else ok <- false
                                        let mR = String.concat "," main
                                        if ok && p = tokens.Length && mR.Length <= 20 then
                                            sol <- (mR, AS, BS, CS); found <- true

    let (mR, fA, fB, fC) = sol
    let prog2 = Array.copy prog
    prog2.[0] <- 2L
    let comp2 = Computer(prog2)
    for s in [mR; fA; fB; fC; "n"] do
        for c in s do comp2.Input.Enqueue(int64 c)
        comp2.Input.Enqueue(10L)
    comp2.Run() |> ignore
    let mutable last = 0L
    while comp2.Output.Count > 0 do last <- comp2.Output.Dequeue()
    printfn "Part Two: %d" last
    0
