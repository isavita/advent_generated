
open System
open System.IO

let [<Literal>] MAX_H = 200
let [<Literal>] MAX_W = 150
let [<Literal>] MAX_P = 8192

let map = Array2D.create MAX_H MAX_W ' '
let path = Array.zeroCreate MAX_P
let mutable width = 0
let mutable height = 0

let wrap r c d =
    match d, r, c with
    | 3, 0, c when c >= 50 && c < 100   -> c + 100, 0, 0
    | 2, r, 50 when r >= 0 && r < 50   -> 149 - r, 0, 0
    | 3, 0, c when c >= 100 && c < 150 -> 199, c - 100, 3
    | 0, r, 149 when r >= 0 && r < 50  -> 149 - r, 99, 2
    | 1, 49, c when c >= 100 && c < 150 -> c - 50, 99, 2
    | 0, r, 99 when r >= 50 && r < 100  -> 49, r + 50, 3
    | 2, r, 50 when r >= 50 && r < 100  -> 100, r - 50, 1
    | 2, r, 0 when r >= 100 && r < 150  -> 149 - r, 50, 0
    | 3, 100, c when c >= 0 && c < 50    -> c + 50, 50, 0
    | 0, r, 99 when r >= 100 && r < 150 -> 149 - r, 149, 2
    | 1, 149, c when c >= 50 && c < 100 -> c + 100, 49, 2
    | 0, r, 49 when r >= 150 && r < 200 -> 149, r - 100, 3
    | 1, 199, c when c >= 0 && c < 50     -> 0, c + 100, 1
    | 2, r, 0 when r >= 150 && r < 200   -> 0, r - 100, 1
    | _ -> r, c, d

[<EntryPoint>]
let main _ =
    use sr = new StreamReader("input.txt")
    let mutable line = sr.ReadLine()
    while not (isNull line) && line.Length > 0 do
        if line.Length > width then width <- line.Length
        for i in 0..line.Length-1 do map.[height, i] <- line.[i]
        height <- height + 1
        line <- sr.ReadLine()
    let pathLine = sr.ReadLine()
    pathLine.CopyTo(0, path, 0, pathLine.Length)
    let pathLen = pathLine.Length

    let mutable r = 0
    let mutable c = 0
    let mutable d = 0
    while map.[r, c] <> '.' do c <- c + 1

    let dr = [| 0; 1; 0; -1 |]
    let dc = [| 1; 0; -1; 0 |]

    let mutable p = 0
    while p < pathLen do
        if Char.IsDigit path.[p] then
            let mutable steps = 0
            while p < pathLen && Char.IsDigit path.[p] do
                steps <- steps * 10 + int path.[p] - int '0'
                p <- p + 1
            for _ in 1..steps do
                let mutable nr = r + dr.[d]
                let mutable nc = c + dc.[d]
                let mutable nd = d
                if nr < 0 || nr >= height || nc < 0 || nc >= width || map.[nr, nc] = ' ' then
                    let nr', nc', nd' = wrap r c d
                    nr <- nr'; nc <- nc'; nd <- nd'
                if map.[nr, nc] = '#' then ()
                else r <- nr; c <- nc; d <- nd
        else
            if path.[p] = 'R' then d <- (d + 1) % 4
            elif path.[p] = 'L' then d <- (d + 3) % 4
            p <- p + 1

    printfn "%d" (1000 * (r + 1) + 4 * (c + 1) + d)
    0
