
open System.IO

[<EntryPoint>]
let main _ =
    let grid = File.ReadAllLines "input.txt"
    let rows = grid.Length
    let mutable x = grid.[0] |> Seq.findIndex ((=) '|')
    let mutable y = 0
    let mutable dx = 0
    let mutable dy = 1
    let mutable steps = 0
    let mutable running = true
    while running do
        if y < 0 || y >= rows || x < 0 || x >= grid.[y].Length then running <- false
        else
            let cell = grid.[y].[x]
            if cell = ' ' then running <- false
            else
                steps <- steps + 1
                if cell = '+' then
                    if dx = 0 then
                        let left = if x > 0 then grid.[y].[x-1] else ' '
                        dx <- if left = '-' || (left >= 'A' && left <= 'Z') then -1 else 1
                        dy <- 0
                    else
                        let up = if y > 0 && x < grid.[y-1].Length then grid.[y-1].[x] else ' '
                        dy <- if up = '|' || (up >= 'A' && up <= 'Z') then -1 else 1
                        dx <- 0
                x <- x + dx
                y <- y + dy
    printfn "%d" steps
    0
