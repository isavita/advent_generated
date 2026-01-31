
open System
open System.IO

type Boundary = { Min:int; Max:int }

let readInput path =
    let lines = File.ReadAllLines path
    let split = Array.findIndex System.String.IsNullOrEmpty lines
    let map = lines[..split-1]
    let path = lines[split+1]
    let maxW = map |> Array.map (fun s -> s.Length) |> Array.max
    let grid = 
        map 
        |> Array.map (fun s -> 
            let sb = System.Text.StringBuilder(s)
            while sb.Length < maxW do sb.Append(' ') |> ignore
            sb.ToString().ToCharArray())
    let h = grid.Length
    let w = maxW
    let rowB = 
        Array.init h (fun y ->
            let mutable mi, ma = -1, -1
            for x in 0..w-1 do
                if grid[y][x] <> ' ' then
                    if mi = -1 then mi <- x
                    ma <- x
            {Min=mi; Max=ma})
    let colB = 
        Array.init w (fun x ->
            let mutable mi, ma = -1, -1
            for y in 0..h-1 do
                if grid[y][x] <> ' ' then
                    if mi = -1 then mi <- y
                    ma <- y
            {Min=mi; Max=ma})
    (grid, rowB, colB, path)

let run () =
    let grid, rowB, colB, path = readInput "input.txt"
    let h = grid.Length
    let w = grid[0].Length
    let startX = 
        let mutable x = 0
        while grid[0][x] <> '.' do x <- x + 1
        x
    let mutable x, y, d = startX, 0, 0
    let dx = [|1;0;-1;0|]
    let dy = [|0;1;0;-1|]
    let rec parse i =
        if i >= path.Length then ()
        else
            if System.Char.IsDigit path[i] then
                let mutable steps = 0
                let mutable j = i
                while j < path.Length && System.Char.IsDigit path[j] do
                    steps <- steps * 10 + int path[j] - int '0'
                    j <- j + 1
                for _ in 1..steps do
                    let nx = x + dx[d]
                    let ny = y + dy[d]
                    let nx, ny =
                        if d = 0 && nx > rowB[y].Max then (rowB[y].Min, y)
                        elif d = 2 && nx < rowB[y].Min then (rowB[y].Max, y)
                        elif d = 1 && ny > colB[x].Max then (x, colB[x].Min)
                        elif d = 3 && ny < colB[x].Min then (x, colB[x].Max)
                        else (nx, ny)
                    if grid[ny][nx] = '#' then ()
                    else 
                        x <- nx
                        y <- ny
                parse j
            else
                d <- 
                    if path[i] = 'R' then (d + 1) % 4
                    else (d + 3) % 4
                parse (i+1)
    parse 0
    printfn "%d" (1000 * (y+1) + 4 * (x+1) + d)

[<EntryPoint>]
let main _ = run(); 0
