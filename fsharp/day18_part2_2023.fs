
open System
open System.IO

let hexToInt (s:string) =
    Convert.ToInt64(s, 16)

let parseLine (line:string) =
    let color = line.Split(' ')[2]
    let dir = color[7]
    let len = hexToInt (color.Substring(2,5))
    match dir with
    | '3' ->  0L, -len
    | '2' -> -len,  0L
    | '1' ->  0L,  len
    | '0' ->  len,  0L
    | _   -> failwith "bad dir"

let readInput() = File.ReadAllLines "input.txt" |> Array.map parseLine

let shoelace (moves:(int64*int64)[]) =
    let mutable x, y, area = 0L, 0L, 0L
    let inline abs x = if x < 0L then -x else x
    for dx, dy in moves do
        let nx = x + dx
        let ny = y + dy
        area <- area + x * ny - nx * y
        x <- nx
        y <- ny
    abs area / 2L

let perimeter (moves:(int64*int64)[]) =
    let mutable len = 0L
    let inline abs x = if x < 0L then -x else x
    for dx, dy in moves do
        len <- len + abs dx + abs dy
    len

[<EntryPoint>]
let main _ =
    let moves = readInput()
    let area = shoelace moves
    let perim = perimeter moves
    printfn "%d" (area + perim / 2L + 1L)
    0
