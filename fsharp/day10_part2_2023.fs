
open System
open System.IO

type Coord = { x: int; y: int }

let coordAdd (c1: Coord) (c2: Coord) = { x = c1.x + c2.x; y = c1.y + c2.y }
let coordSubtract (c1: Coord) (c2: Coord) = { x = c1.x - c2.x; y = c1.y - c2.y }
let coordOpposite (c: Coord) = { x = -c.x; y = -c.y }

type Tile = 
    | Empty 
    | Start 
    | Vertical 
    | Horizontal 
    | TopLeftCorner 
    | TopRightCorner 
    | BottomLeftCorner 
    | BottomRightCorner 
    | Enclosed

type Pipe = { top: bool; right: bool; bottom: bool; left: bool }

let tileToPipe (tile: Tile) = 
    match tile with
    | Vertical -> { top = true; right = false; bottom = true; left = false }
    | Horizontal -> { top = false; right = true; bottom = false; left = true }
    | TopLeftCorner -> { top = true; right = false; bottom = false; left = true }
    | TopRightCorner -> { top = true; right = true; bottom = false; left = false }
    | BottomLeftCorner -> { top = false; right = false; bottom = true; left = true }
    | BottomRightCorner -> { top = false; right = true; bottom = true; left = false }
    | _ -> { top = false; right = false; bottom = false; left = false }

let pipeToTile (pipe: Pipe) = 
    if pipe = { top = true; right = false; bottom = true; left = false } then Vertical 
    elif pipe = { top = false; right = true; bottom = false; left = true } then Horizontal 
    elif pipe = { top = true; right = false; bottom = false; left = true } then TopLeftCorner 
    elif pipe = { top = true; right = true; bottom = false; left = false } then TopRightCorner 
    elif pipe = { top = false; right = false; bottom = true; left = true } then BottomLeftCorner 
    elif pipe = { top = false; right = true; bottom = true; left = false } then BottomRightCorner 
    else Empty

let getPipeFromNeighbors (c: Coord) (grid: Tile[,]) = 
    let possibleNeighbors = 
        [| coordAdd c { x = 0; y = -1 }; 
           coordAdd c { x = 1; y = 0 }; 
           coordAdd c { x = 0; y = 1 }; 
           coordAdd c { x = -1; y = 0 } |]
    let dirs = [| { x = 0; y = -1 }; { x = 1; y = 0 }; { x = 0; y = 1 }; { x = -1; y = 0 } |]
    let mutable pipe = { top = false; right = false; bottom = false; left = false }
    for i = 0 to 3 do
        let neighborCoord = possibleNeighbors.[i]
        if neighborCoord.x >= 0 && neighborCoord.x < Array2D.length1 grid && neighborCoord.y >= 0 && neighborCoord.y < Array2D.length2 grid then
            let neighborPipe = tileToPipe grid.[neighborCoord.x, neighborCoord.y]
            let opp = coordOpposite dirs.[i]
            if (opp.x = 0 && opp.y = -1 && neighborPipe.top) ||
               (opp.x = 1 && opp.y = 0 && neighborPipe.right) ||
               (opp.x = 0 && opp.y = 1 && neighborPipe.bottom) ||
               (opp.x = -1 && opp.y = 0 && neighborPipe.left) then
                if dirs.[i].x = 0 && dirs.[i].y = -1 then pipe <- { pipe with top = true }
                elif dirs.[i].x = 1 && dirs.[i].y = 0 then pipe <- { pipe with right = true }
                elif dirs.[i].x = 0 && dirs.[i].y = 1 then pipe <- { pipe with bottom = true }
                elif dirs.[i].x = -1 && dirs.[i].y = 0 then pipe <- { pipe with left = true }
    pipe

let findStart (grid: Tile[,]) = 
    let mutable start = { x = 0; y = 0 }
    for y = 0 to Array2D.length2 grid - 1 do
        for x = 0 to Array2D.length1 grid - 1 do
            if grid.[x, y] = Start then start <- { x = x; y = y }
    start

let pathFinding (start: Coord) (grid: Tile[,]) = 
    let path = ResizeArray<_>()
    path.Add start
    let startPipe = getPipeFromNeighbors start grid
    let mutable previousDir = 
        if startPipe.top then { x = 0; y = -1 }
        elif startPipe.right then { x = 1; y = 0 }
        elif startPipe.bottom then { x = 0; y = 1 }
        else { x = -1; y = 0 }
    let mutable current = coordAdd start previousDir
    while current.x <> start.x || current.y <> start.y do
        path.Add current
        let currentPipe = tileToPipe grid.[current.x, current.y]
        if currentPipe.top && (previousDir.x <> 0 || previousDir.y <> 1) then 
            previousDir <- { x = 0; y = -1 }
            current <- coordAdd current previousDir
        elif currentPipe.right && (previousDir.x <> -1 || previousDir.y <> 0) then 
            previousDir <- { x = 1; y = 0 }
            current <- coordAdd current previousDir
        elif currentPipe.bottom && (previousDir.x <> 0 || previousDir.y <> -1) then 
            previousDir <- { x = 0; y = 1 }
            current <- coordAdd current previousDir
        else 
            previousDir <- { x = -1; y = 0 }
            current <- coordAdd current previousDir
    path |> Seq.toArray

let isInside (c: Coord) (grid: Tile[,]) = 
    if grid.[c.x, c.y] <> Empty then false 
    else
        let mutable numPipeOnLeft = 0
        let mutable startPipe = Unchecked.defaultof<_>
        for x = 0 to c.x - 1 do
            let v = grid.[x, c.y]
            match v with
            | Vertical -> numPipeOnLeft <- numPipeOnLeft + 1
            | TopRightCorner -> startPipe <- TopRightCorner
            | BottomRightCorner -> startPipe <- BottomRightCorner
            | TopLeftCorner -> 
                if startPipe = BottomRightCorner then 
                    startPipe <- Unchecked.defaultof<_>
                    numPipeOnLeft <- numPipeOnLeft + 1
            | BottomLeftCorner -> 
                if startPipe = TopRightCorner then 
                    startPipe <- Unchecked.defaultof<_>
                    numPipeOnLeft <- numPipeOnLeft + 1
            | _ -> ()
        numPipeOnLeft % 2 = 1

let getPathGrid (grid: Tile[,]) (path: Coord[]) = 
    let newGrid = Array2D.init (Array2D.length1 grid) (Array2D.length2 grid) (fun _ _ -> Empty)
    for coord in path do
        newGrid.[coord.x, coord.y] <- grid.[coord.x, coord.y]
    let start = path.[0]
    newGrid.[start.x, start.y] <- pipeToTile (getPipeFromNeighbors start grid)
    newGrid

let solve (lines: string[]) = 
    let grid = 
        Array2D.init (lines.[0].Length) lines.Length (fun x y -> 
            match lines.[y].[x] with
            | '.' -> Empty
            | 'S' -> Start
            | '|' -> Vertical
            | '-' -> Horizontal
            | 'J' -> TopLeftCorner
            | 'L' -> TopRightCorner
            | '7' -> BottomLeftCorner
            | 'F' -> BottomRightCorner
            | _ -> failwith "Invalid character")
    let start = findStart grid
    let path = pathFinding start grid
    let pathGrid = getPathGrid grid path
    let mutable cnt = 0
    for y = 0 to Array2D.length2 pathGrid - 1 do
        for x = 0 to Array2D.length1 pathGrid - 1 do
            let c = { x = x; y = y }
            if isInside c pathGrid then cnt <- cnt + 1
    cnt

let main () = 
    let file = "input.txt"
    let lines = File.ReadAllLines file
    let result = solve lines
    printfn "%d" result

main ()
