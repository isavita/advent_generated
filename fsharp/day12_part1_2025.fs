
open System
open System.IO
open System.Collections.Generic

type Point = { X: int; Y: int }
type Orientation = { Points: Point[]; W: int; H: int }
type Shape = 
    { Orientations: Orientation[]; Area: int }
    static member Create(rows: string list) =
        let mutable pts = []
        for r in 0 .. rows.Length - 1 do
            let row = rows.[r]
            for c in 0 .. row.Length - 1 do
                if row.[c] = '#' then pts <- { X = c; Y = r } :: pts
        let area = pts.Length
        let mutable unique = Set.empty
        for i in 0 .. 7 do
            let transformed = pts |> List.map (fun p ->
                match i with
                | 0 -> { X = p.X; Y = p.Y } | 1 -> { X = p.Y; Y = -p.X }
                | 2 -> { X = -p.X; Y = -p.Y } | 3 -> { X = -p.Y; Y = p.X }
                | 4 -> { X = -p.X; Y = p.Y } | 5 -> { X = p.Y; Y = p.X }
                | 6 -> { X = p.X; Y = -p.Y } | _ -> { X = -p.Y; Y = -p.X })
            let mx = transformed |> List.map (fun p -> p.X) |> List.min
            let my = transformed |> List.map (fun p -> p.Y) |> List.min
            let normalized = transformed |> List.map (fun p -> { X = p.X - mx; Y = p.Y - my }) |> List.sort
            unique <- Set.add normalized unique
        let orientations = unique |> Set.toList |> List.map (fun pts ->
            let mw = pts |> List.map (fun p -> p.X) |> List.max
            let mh = pts |> List.map (fun p -> p.Y) |> List.max
            { Points = pts |> List.toArray; W = mw + 1; H = mh + 1 }) |> List.toArray
        { Orientations = orientations; Area = area }

let solve pToFit W H totalArea =
    let grid = Array.create (W * H) false
    let pWithOff = pToFit |> Array.map (fun s ->
        let ors = s.Orientations |> Array.map (fun o -> (o.W, o.H, o.Points |> Array.map (fun p -> p.Y * W + p.X)))
        (s.Area, ors))
    let rec go idx rem free =
        if idx = pWithOff.Length then true
        elif rem > free then false
        else
            let sArea, ors = pWithOff.[idx]
            let mutable found = false
            let mutable o = 0
            while o < ors.Length && not found do
                let oW, oH, off = ors.[o]
                if oW <= W && oH <= H then
                    for r in 0 .. H - oH do
                        let rOff = r * W
                        for c in 0 .. W - oW do
                            if not found then
                                let mutable ok, p, bp = true, 0, rOff + c
                                while p < off.Length && ok do
                                    if grid.[bp + off.[p]] then ok <- false
                                    p <- p + 1
                                if ok then
                                    for k in 0 .. off.Length - 1 do grid.[bp + off.[k]] <- true
                                    if go (idx + 1) (rem - sArea) (free - sArea) then found <- true
                                    else for k in 0 .. off.Length - 1 do grid.[bp + off.[k]] <- false
                o <- o + 1
            found
    go 0 totalArea (W * H)

[<EntryPoint>]
let main _ =
    if File.Exists "input.txt" then
        let lines = File.ReadAllLines "input.txt"
        let allShapes = ResizeArray<Shape>()
        let mutable total, i = 0, 0
        while i < lines.Length do
            let t = lines.[i].Trim()
            if t = "" then i <- i + 1
            elif t.Contains "x" && t.Contains ":" then
                let parts = t.Split ':'
                let d = parts.[0].Trim().Split 'x'
                let W, H = int (d.[0].Trim()), int (d.[1].Trim())
                let counts = parts.[1].Trim().Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
                let pToFit = ResizeArray<Shape>()
                let mutable area = 0
                for j in 0 .. counts.Length - 1 do
                    if j < allShapes.Count then
                        for _ in 1 .. int counts.[j] do
                            pToFit.Add allShapes.[j]; area <- area + allShapes.[j].Area
                if solve (pToFit |> Seq.sortByDescending (fun s -> s.Area) |> Seq.toArray) W H area then total <- total + 1
                i <- i + 1
            elif t.EndsWith ":" then
                i <- i + 1
                let mutable rows, cont = [], true
                while i < lines.Length && cont do
                    let r = lines.[i].Trim()
                    if r = "" || r.Contains ":" then cont <- false
                    elif r.Contains "#" || r.Contains "." then rows <- r :: rows; i <- i + 1
                    else cont <- false
                if not rows.IsEmpty then allShapes.Add(Shape.Create(List.rev rows))
            else i <- i + 1
        printfn "%d" total
    0

