
open System
open System.IO

let GEOLOGIC_Y = 16807L
let GEOLOGIC_X = 48271L
let CAVE_MODULO = 20183

let TYPE_ROCKY = 0
let TYPE_WET = 1
let TYPE_NARROW = 2

let TOOL_NONE = 1
let TOOL_TORCH = 2
let TOOL_GEAR = 4
let MAX_TOOL_VALUE = 4

let PADDING = 100
let INF = Int32.MaxValue / 4

let lines = File.ReadAllLines("input.txt")
let depth = int (lines.[0].Split(':').[1].Trim())
let tPart = lines.[1].Split(':').[1].Trim()
let coords = tPart.Split(',')
let targetX = int coords.[0]
let targetY = int coords.[1]

let boundX = targetX + PADDING
let boundY = targetY + PADDING

let erosionCache = Array2D.init boundY boundX (fun _ _ -> -1)

let rec GetErosionLevel x y =
    if x < 0 || y < 0 || x >= boundX || y >= boundY then -1
    elif erosionCache.[y, x] <> -1 then erosionCache.[y, x]
    else
        let geoIndex =
            if x = 0 && y = 0 then 0L
            elif x = targetX && y = targetY then 0L
            elif y = 0 then (int64 x) * GEOLOGIC_Y
            elif x = 0 then (int64 y) * GEOLOGIC_X
            else
                let erosionLeft = GetErosionLevel (x-1) y
                let erosionUp = GetErosionLevel x (y-1)
                (int64 erosionLeft) * (int64 erosionUp)

        let level = int ((geoIndex + int64 depth) % int64 CAVE_MODULO)
        erosionCache.[y, x] <- level
        level

for y in 0..boundY-1 do
    for x in 0..boundX-1 do
        GetErosionLevel x y |> ignore

let GetType x y = GetErosionLevel x y % 3

let AllowedTools regionType =
    match regionType with
    | 0 -> TOOL_GEAR ||| TOOL_TORCH
    | 1 -> TOOL_GEAR ||| TOOL_NONE
    | 2 -> TOOL_TORCH ||| TOOL_NONE
    | _ -> 0

let dist = Array3D.init boundY boundX (MAX_TOOL_VALUE + 1) (fun _ _ _ -> INF)

type State =
    { Time: int; X: int; Y: int; Tool: int }

type MinHeap() =
    let mutable data = []
    member _.Count = data.Length
    member _.Push(s) =
        data <- (s::data) |> List.sortBy (fun x -> x.Time)
    member _.Pop() =
        match data with
        | [] -> failwith "Empty heap"
        | h::t -> data <- t; h

dist.[0, 0, TOOL_TORCH] <- 0
let heap = MinHeap()
heap.Push({ Time = 0; X = 0; Y = 0; Tool = TOOL_TORCH })

let dx = [| 0; 0; 1; -1 |]
let dy = [| 1; -1; 0; 0 |]

let mutable finalTime = -1

while heap.Count > 0 && finalTime = -1 do
    let cur = heap.Pop()
    if cur.Time <= dist.[cur.Y, cur.X, cur.Tool] then
        if cur.X = targetX && cur.Y = targetY && cur.Tool = TOOL_TORCH then
            finalTime <- cur.Time
        else
            let regionType = GetType cur.X cur.Y
            let allowedHere = AllowedTools regionType

            for nextTool in [| TOOL_NONE; TOOL_TORCH; TOOL_GEAR |] do
                if (nextTool &&& allowedHere) <> 0 && nextTool <> cur.Tool then
                    let nt = cur.Time + 7
                    if nt < dist.[cur.Y, cur.X, nextTool] then
                        dist.[cur.Y, cur.X, nextTool] <- nt
                        heap.Push({ Time = nt; X = cur.X; Y = cur.Y; Tool = nextTool })

            for dir in 0..3 do
                let nx = cur.X + dx.[dir]
                let ny = cur.Y + dy.[dir]
                if nx >= 0 && ny >= 0 && nx < boundX && ny < boundY then
                    let neighborType = GetType nx ny
                    let allowedThere = AllowedTools neighborType
                    if (cur.Tool &&& allowedThere) <> 0 then
                        let nt = cur.Time + 1
                        if nt < dist.[ny, nx, cur.Tool] then
                            dist.[ny, nx, cur.Tool] <- nt
                            heap.Push({ Time = nt; X = nx; Y = ny; Tool = cur.Tool })

printfn "%d" finalTime
