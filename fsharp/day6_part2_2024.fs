
open System.IO

let dirs = [| (0, -1); (1, 0); (0, 1); (-1, 0) |]

let loops (grid: char[,]) sx sy sdir (h, w) =
    let seen = Array.zeroCreate (h * w * 4)
    let rec loop x y dir step =
        if step >= 2000000 then false
        else
            let idx = (y * w + x) * 4 + dir
            if seen.[idx] then true
            else
                seen.[idx] <- true
                let dx, dy = dirs.[dir]
                let nx, ny = x + dx, y + dy
                if nx < 0 || nx >= w || ny < 0 || ny >= h then false
                elif grid.[ny, nx] = '#' then loop x y ((dir + 1) % 4) (step + 1)
                else loop nx ny dir (step + 1)
    loop sx sy sdir 0

let grid = File.ReadAllLines "input.txt" |> array2D
let h, w = Array2D.length1 grid, Array2D.length2 grid
let mutable sx, sy, sdir = 0, 0, 0
for y in 0..h-1 do
    for x in 0..w-1 do
        match grid.[y, x] with
        | '^' -> sx <- x; sy <- y; sdir <- 0
        | '>' -> sx <- x; sy <- y; sdir <- 1
        | 'v' -> sx <- x; sy <- y; sdir <- 2
        | '<' -> sx <- x; sy <- y; sdir <- 3
        | _ -> ()
grid.[sy, sx] <- '.'

let mutable count = 0
for y in 0..h-1 do
    for x in 0..w-1 do
        if (x <> sx || y <> sy) && grid.[y, x] = '.' then
            grid.[y, x] <- '#'
            if loops grid sx sy sdir (h, w) then count <- count + 1
            grid.[y, x] <- '.'
printfn "%d" count
