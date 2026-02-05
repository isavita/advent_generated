open System
open System.IO

type Pos = int * int

let findStart (grid: char[][]) : Pos =
    let mutable res: Pos option = None
    for r in 0 .. grid.Length - 1 do
        for c in 0 .. grid.[r].Length - 1 do
            match res with
            | None when grid.[r].[c] = 'S' -> res <- Some (r, c)
            | _ -> ()
    match res with
    | Some p -> p
    | None -> failwith "No start"

let inSet (s: string) (ch: char) = s.IndexOf ch >= 0

let getNext (pos: Pos) (prev: Pos option) (grid: char[][]) (rows: int) (cols: int) (start: Pos) : Pos option =
    let (r, c) = pos
    let ch = grid.[r].[c]
    let mutable cands: (int*int) list = []
    match ch with
    | 'S' ->
        if r > 0 && inSet "|7F" grid.[r - 1].[c] then cands <- cands @ [(r - 1, c)]
        if r < rows - 1 && inSet "|LJ" grid.[r + 1].[c] then cands <- cands @ [(r + 1, c)]
        if c > 0 && inSet "-LF" grid.[r].[c - 1] then cands <- cands @ [(r, c - 1)]
        if c < cols - 1 && inSet "-J7" grid.[r].[c + 1] then cands <- cands @ [(r, c + 1)]
    | '|' ->
        if r > 0 then cands <- cands @ [(r - 1, c)]
        if r < rows - 1 then cands <- cands @ [(r + 1, c)]
    | '-' ->
        if c > 0 then cands <- cands @ [(r, c - 1)]
        if c < cols - 1 then cands <- cands @ [(r, c + 1)]
    | 'L' ->
        if r > 0 then cands <- cands @ [(r - 1, c)]
        if c < cols - 1 then cands <- cands @ [(r, c + 1)]
    | 'J' ->
        if r > 0 then cands <- cands @ [(r - 1, c)]
        if c > 0 then cands <- cands @ [(r, c - 1)]
    | '7' ->
        if r < rows - 1 then cands <- cands @ [(r + 1, c)]
        if c > 0 then cands <- cands @ [(r, c - 1)]
    | 'F' ->
        if r < rows - 1 then cands <- cands @ [(r + 1, c)]
        if c < cols - 1 then cands <- cands @ [(r, c + 1)]
    | _ -> ()

    match prev with
    | Some p -> cands |> List.tryFind (fun cand -> cand <> p)
    | None -> match cands with | [] -> None | h::_ -> Some h

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines "input.txt"
    let grid = lines |> Array.map (fun l -> l.ToCharArray())
    let rows = grid.Length
    let cols = grid.[0].Length
    let start = findStart grid
    let mutable loop = new System.Collections.Generic.List<Pos>()
    let mutable current = start
    let mutable prev: Pos option = None
    let mutable cont = true
    while cont do
        loop.Add current
        let next = getNext current prev grid rows cols start
        match next with
        | None -> cont <- false
        | Some n ->
            prev <- Some current
            current <- n
            if current = start then cont <- false
    let ans = loop.Count / 2
    printfn "%d" ans
    0