
open System.IO

let dirs = [| (1,0); (-1,0); (0,1); (0,-1) |]

let grid = File.ReadAllLines "input.txt" |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))
let nr,nc = grid.Length,grid.[0].Length
let dp = Array.init nr (fun _ -> Array.create nc -1L)

let rec dfs r c =
    match dp.[r].[c] with
    | -1L ->
        let h = grid.[r].[c]
        if h = 9 then
            dp.[r].[c] <- 1L
            1L
        else
            let mutable sum = 0L
            for dr,dc in dirs do
                let nr2,nc2 = r+dr,c+dc
                if nr2>=0 && nr2<nr && nc2>=0 && nc2<nc && grid.[nr2].[nc2]=h+1 then
                    sum <- sum + dfs nr2 nc2
            dp.[r].[c] <- sum
            sum
    | v -> v

let mutable total = 0L
for r in 0..nr-1 do
    for c in 0..nc-1 do
        if grid.[r].[c]=0 then total <- total + dfs r c
printfn "%d" total
