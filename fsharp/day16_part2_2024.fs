
open System
open System.IO
open System.Collections.Generic

let dx = [| -1; 0; 1; 0 |]
let dy = [| 0; 1; 0; -1 |]

let grid = File.ReadAllLines "input.txt" |> Array.map (fun s -> s.ToCharArray())
let n = grid.Length
let m = if n = 0 then 0 else grid.[0].Length

let mutable sx, sy, ex, ey = -1, -1, -1, -1
for i in 0 .. n - 1 do
    for j in 0 .. m - 1 do
        if grid.[i].[j] = 'S' then sx <- i; sy <- j
        elif grid.[i].[j] = 'E' then ex <- i; ey <- j

let inf = System.Int32.MaxValue
let dist = Array3D.create n m 4 inf
let vis = Array3D.create n m 4 false
let used = Array2D.create n m false

let q = PriorityQueue<int * int * int * int, int>()
dist.[sx, sy, 1] <- 0
q.Enqueue((0, sx, sy, 1), 0)

while q.Count > 0 do
    let (cost, x, y, d) = q.Dequeue()
    if cost = dist.[x, y, d] then
        for nd in [| (d + 1) % 4; (d + 3) % 4 |] do
            let nc = cost + 1000
            if nc < dist.[x, y, nd] then
                dist.[x, y, nd] <- nc
                q.Enqueue((nc, x, y, nd), nc)
        let nx = x + dx.[d]
        let ny = y + dy.[d]
        if nx >= 0 && nx < n && ny >= 0 && ny < m && grid.[nx].[ny] <> '#' then
            let nc = cost + 1
            if nc < dist.[nx, ny, d] then
                dist.[nx, ny, d] <- nc
                q.Enqueue((nc, nx, ny, d), nc)

let best = Array.min [| for d in 0 .. 3 -> dist.[ex, ey, d] |]

if best = inf then
    printfn "0"
else
    let stack = Stack<int * int * int>()
    for d in 0 .. 3 do
        if dist.[ex, ey, d] = best then
            vis.[ex, ey, d] <- true
            stack.Push(ex, ey, d)

    while stack.Count > 0 do
        let x, y, d = stack.Pop()
        used.[x, y] <- true
        let costU = dist.[x, y, d]
        for pd in [| (d + 1) % 4; (d + 3) % 4 |] do
            if costU >= 1000 && dist.[x, y, pd] = costU - 1000 && not vis.[x, y, pd] then
                vis.[x, y, pd] <- true
                stack.Push(x, y, pd)
        let px = x - dx.[d]
        let py = y - dy.[d]
        if px >= 0 && px < n && py >= 0 && py < m && grid.[px].[py] <> '#' then
            if costU > 0 && dist.[px, py, d] = costU - 1 && not vis.[px, py, d] then
                vis.[px, py, d] <- true
                stack.Push(px, py, d)

    let mutable cnt = 0
    for i in 0 .. n - 1 do
        for j in 0 .. m - 1 do
            if used.[i, j] then cnt <- cnt + 1
    printfn "%d" cnt
