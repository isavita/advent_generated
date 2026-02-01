
open System
open System.IO

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let mutable x = lines.[0].IndexOf '|'
    let mutable y = 0
    let mutable dx = 0
    let mutable dy = 1
    let letters = ResizeArray<char>()
    let mutable finished = false
    while not finished && x >= 0 && x < lines.[0].Length && y >= 0 && y < lines.Length do
        let cell = lines.[y].[x]
        if cell = ' ' then finished <- true
        else
            if cell >= 'A' && cell <= 'Z' then letters.Add cell
            if cell = '+' then
                if dx = 0 then
                    if x > 0 && (lines.[y].[x-1] = '-' || (lines.[y].[x-1] >= 'A' && lines.[y].[x-1] <= 'Z')) then
                        dx <- -1; dy <- 0
                    else
                        dx <- 1; dy <- 0
                else
                    if y > 0 && (lines.[y-1].[x] = '|' || (lines.[y-1].[x] >= 'A' && lines.[y-1].[x] <= 'Z')) then
                        dx <- 0; dy <- -1
                    else
                        dx <- 0; dy <- 1
            x <- x + dx
            y <- y + dy
    printfn "%s" (String.Concat(letters))
    0
