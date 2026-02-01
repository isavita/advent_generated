
open System
open System.IO

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let rows = lines.Length
    let cols = lines.[0].Length
    let grid = Array2D.init rows cols (fun r c -> lines.[r].[c])
    let startX = lines.[0] |> Seq.findIndex ((=) '.')
    let visited = Array2D.create rows cols false
    let mutable maxSteps = 0
    let rec dfs r c steps =
        if r < 0 || r >= rows || c < 0 || c >= cols || grid.[r,c] = '#' || visited.[r,c] then ()
        elif r = rows - 1 then if steps > maxSteps then maxSteps <- steps
        else
            visited.[r,c] <- true
            match grid.[r,c] with
            | '.' ->
                dfs (r+1) c (steps+1)
                dfs (r-1) c (steps+1)
                dfs r (c+1) (steps+1)
                dfs r (c-1) (steps+1)
            | 'v' -> dfs (r+1) c (steps+1)
            | '^' -> dfs (r-1) c (steps+1)
            | '>' -> dfs r (c+1) (steps+1)
            | '<' -> dfs r (c-1) (steps+1)
            | _ -> ()
            visited.[r,c] <- false
    dfs 0 startX 0
    printfn "%d" maxSteps
    0
