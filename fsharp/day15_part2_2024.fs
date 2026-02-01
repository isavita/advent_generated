
open System
open System.IO

let readInput () = 
    let txt = File.ReadAllText "input.txt"
    let parts = txt.Split([|"\n\n"; "\r\n\r\n"|], StringSplitOptions.RemoveEmptyEntries)
    if parts.Length < 2 then "","" else parts.[0], parts.[1]

let prepareGrid (gStr: string) p2 =
    let s = if p2 then gStr.Replace("#","##").Replace(".","..").Replace("O","[]").Replace("@","@.") else gStr
    s.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun l -> l.ToCharArray())

let findRobot (g: char[][]) =
    let mutable r, c = -1, -1
    for i in 0 .. g.Length - 1 do
        for j in 0 .. g.[0].Length - 1 do
            if g.[i].[j] = '@' then r <- i; c <- j
    r, c

let rec canMove (g: char[][]) r c dr dc =
    let curr = g.[r].[c]
    if curr = '.' then true
    elif curr = '#' then false
    elif dr <> 0 then
        if curr = '[' then canMove g (r+dr) c dr dc && canMove g (r+dr) (c+1) dr dc
        elif curr = ']' then canMove g (r+dr) c dr dc && canMove g (r+dr) (c-1) dr dc
        else canMove g (r+dr) (c+dc) dr dc
    else canMove g (r+dr) (c+dc) dr dc

let rec move (g: char[][]) r c dr dc =
    if g.[r].[c] = '.' then ()
    elif dr <> 0 && (g.[r].[c] = '[' || g.[r].[c] = ']') then
        let cL = if g.[r].[c] = '[' then c else c - 1
        if g.[r].[cL] <> '.' then
            move g (r+dr) cL dr dc
            move g (r+dr) (cL+1) dr dc
            g.[r+dr].[cL] <- '['
            g.[r+dr].[cL+1] <- ']'
            g.[r].[cL] <- '.'
            g.[r].[cL+1] <- '.'
    else
        move g (r+dr) (c+dc) dr dc
        g.[r+dr].[c+dc] <- g.[r].[c]
        g.[r].[c] <- '.'

let solve gStr (mStr: string) p2 =
    let g = prepareGrid gStr p2
    let mutable r, c = findRobot g
    for m in mStr do
        let dr = if m = '^' then -1 elif m = 'v' then 1 else 0
        let dc = if m = '<' then -1 elif m = '>' then 1 else 0
        if dr <> 0 || dc <> 0 then
            if canMove g r c dr dc then
                move g r c dr dc
                r <- r + dr
                c <- c + dc
    let mutable sum = 0L
    for i in 0 .. g.Length - 1 do
        for j in 0 .. g.[0].Length - 1 do
            if g.[i].[j] = 'O' || g.[i].[j] = '[' then sum <- sum + 100L * int64 i + int64 j
    sum

[<EntryPoint>]
let main _ =
    let gStr, mStr = readInput ()
    if gStr <> "" then
        printfn "%d" (solve gStr mStr false)
        printfn "%d" (solve gStr mStr true)
    0
