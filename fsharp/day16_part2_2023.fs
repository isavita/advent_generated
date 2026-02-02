
open System
open System.IO
open System.Collections.Generic

type Direction = 
    | Right = 0 
    | Left = 1 
    | Down = 2 
    | Up = 3

let dirToDxDy = function
    | Direction.Right -> (1, 0)
    | Direction.Left -> (-1, 0)
    | Direction.Down -> (0, 1)
    | Direction.Up -> (0, -1)

let dxDyToDir (dx, dy) =
    match (dx, dy) with
    | (1, 0) -> Direction.Right
    | (-1, 0) -> Direction.Left
    | (0, 1) -> Direction.Down
    | (0, -1) -> Direction.Up
    | _ -> failwith "Invalid direction"

type State = int * int * Direction

let simulateBeam (grid: char[,]) (startX, startY) (startDir: Direction) =
    let height, width = grid.GetLength 0, grid.GetLength 1
    let visited = new HashSet<State>()
    let energized = new HashSet<int * int>()
    let queue = new Queue<State>()
    queue.Enqueue(startX, startY, startDir)

    while queue.Count > 0 do
        let (x, y, dir) = queue.Dequeue()
        let (dx, dy) = dirToDxDy dir
        let nx, ny = x + dx, y + dy

        if nx >= 0 && nx < width && ny >= 0 && ny < height then
            if visited.Add(nx, ny, dir) then
                energized.Add(ny, nx) |> ignore

                match grid[ny, nx] with
                | '.' -> queue.Enqueue(nx, ny, dir)
                | '/' ->
                    let (ndx, ndy) = (-dy, -dx)
                    let ndir = dxDyToDir (ndx, ndy)
                    queue.Enqueue(nx, ny, ndir)
                | '\\' ->
                    let (ndx, ndy) = (dy, dx)
                    let ndir = dxDyToDir (ndx, ndy)
                    queue.Enqueue(nx, ny, ndir)
                | '|' ->
                    if dx <> 0 then
                        queue.Enqueue(nx, ny, Direction.Down)
                        queue.Enqueue(nx, ny, Direction.Up)
                    else
                        queue.Enqueue(nx, ny, dir)
                | '-' ->
                    if dy <> 0 then
                        queue.Enqueue(nx, ny, Direction.Right)
                        queue.Enqueue(nx, ny, Direction.Left)
                    else
                        queue.Enqueue(nx, ny, dir)
                | _ -> ()

    energized.Count

let main() =
    let lines = File.ReadAllLines("input.txt")
    let height, width = lines.Length, lines[0].Length
    let grid = Array2D.init height width (fun y x -> lines[y].[x])

    let maxEnergized =
        [for x in 0 .. width - 1 -> simulateBeam grid (x, -1) Direction.Down
         for x in 0 .. width - 1 -> simulateBeam grid (x, height) Direction.Up
         for y in 0 .. height - 1 -> simulateBeam grid (-1, y) Direction.Right
         for y in 0 .. height - 1 -> simulateBeam grid (width, y) Direction.Left]
        |> List.max

    printfn "%d" maxEnergized

main()
