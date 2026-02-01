
open System
open System.IO

type Coord = { X:int64; Y:int64; Z:int64 }
type Bot = { Pos:Coord; R:int64 }

let dist a b = abs(a.X - b.X) + abs(a.Y - b.Y) + abs(a.Z - b.Z)

let inRange (bots:Bot list) p =
    bots |> List.filter (fun b -> dist b.Pos p <= b.R) |> List.length

let solve bots =
    let zero = { X=0L; Y=0L; Z=0L }
    let mutable zoom = 1L <<< 30
    let mutable tl = { X=0L; Y=0L; Z=0L }
    let mutable br = { X=0L; Y=0L; Z=0L }
    let mutable best = zero
    while true do
        let sb = bots |> List.map (fun b ->
            { Pos={X=b.Pos.X/zoom; Y=b.Pos.Y/zoom; Z=b.Pos.Z/zoom}; R=b.R/zoom })
        let mutable bestCnt = -1
        for x in tl.X..br.X do
            for y in tl.Y..br.Y do
                for z in tl.Z..br.Z do
                    let cur = { X=x; Y=y; Z=z }
                    let cnt = inRange sb cur
                    if cnt > bestCnt || (cnt = bestCnt && dist zero cur < dist zero best) then
                        best <- cur
                        bestCnt <- cnt
        tl <- { X=(best.X-1L)*2L; Y=(best.Y-1L)*2L; Z=(best.Z-1L)*2L }
        br <- { X=(best.X+1L)*2L; Y=(best.Y+1L)*2L; Z=(best.Z+1L)*2L }
        zoom <- zoom >>> 1
        if zoom = 0L then dist zero best |> printfn "%d"; exit 0

let parse (s:string) =
    let p = s.IndexOf '<'
    let parts = s.Substring(p+1).Split([|','; '>'; 'r'; '='; ' '|], StringSplitOptions.RemoveEmptyEntries)
    { Pos={X=int64 parts.[0]; Y=int64 parts.[1]; Z=int64 parts.[2]}; R=int64 parts.[3] }

File.ReadAllLines "input.txt" |> Array.map parse |> Array.toList |> solve
