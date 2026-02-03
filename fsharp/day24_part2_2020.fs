
open System
open System.IO
open System.Collections.Generic

type Coord = { Q:int; R:int }

let directions = [| (1,0); (0,1); (-1,1); (-1,0); (0,-1); (1,-1) |]

let dirMap = dict [ ("e",0); ("se",1); ("sw",2); ("w",3); ("nw",4); ("ne",5) ]

let parse (line:string) =
    let mutable q = 0
    let mutable r = 0
    let mutable i = 0
    while i < line.Length do
        let dir =
            if line.[i] = 'e' || line.[i] = 'w' then
                let d = string line.[i]
                i <- i + 1
                d
            else
                let d = line.Substring(i,2)
                i <- i + 2
                d
        let idx = dirMap.[dir]
        let dq,dr = directions.[idx]
        q <- q + dq
        r <- r + dr
    { Q = q; R = r }

let neighbors c =
    seq { for dq,dr in directions -> { Q = c.Q + dq; R = c.R + dr } }

[<EntryPoint>]
let main _ =
    let mutable black = HashSet<Coord>()
    for line in File.ReadLines "input.txt" do
        let c = parse line
        if black.Contains c then black.Remove c |> ignore else black.Add c |> ignore
    for _ in 1 .. 100 do
        let toCheck = HashSet<Coord>()
        for tile in black do
            toCheck.Add tile |> ignore
            for nb in neighbors tile do toCheck.Add nb |> ignore
        let newBlack = HashSet<Coord>()
        for tile in toCheck do
            let bc = Seq.filter (fun n -> black.Contains n) (neighbors tile) |> Seq.length
            let isBlack = black.Contains tile
            if (isBlack && (bc = 1 || bc = 2)) || (not isBlack && bc = 2) then
                newBlack.Add tile |> ignore
        black <- newBlack
    printfn "%d" black.Count
    0
