
open System.IO

let dirs = [| (0,-1); (1,0); (0,1); (-1,0) |]

let grid = File.ReadAllLines "input.txt" |> Array.map (fun s -> s.ToCharArray())
let h = grid.Length
let w = grid.[0].Length

let mutable x, y, dirIdx = 0, 0, 0
for i in 0 .. h - 1 do
    for j in 0 .. w - 1 do
        match grid.[i].[j] with
        | '^' -> x <- j; y <- i; dirIdx <- 0
        | '>' -> x <- j; y <- i; dirIdx <- 1
        | 'v' -> x <- j; y <- i; dirIdx <- 2
        | '<' -> x <- j; y <- i; dirIdx <- 3
        | _ -> ()

let visited = Array2D.create h w false
let mutable count = 1
visited.[y,x] <- true

let mutable loop = true
while loop do
    let dx, dy = dirs.[dirIdx]
    let nx, ny = x + dx, y + dy
    if nx < 0 || nx >= w || ny < 0 || ny >= h then
        loop <- false
    else
        if grid.[ny].[nx] = '#' then
            dirIdx <- (dirIdx + 1) % 4
        else
            x <- nx
            y <- ny
            if not visited.[y,x] then
                visited.[y,x] <- true
                count <- count + 1

printfn "%d" count
