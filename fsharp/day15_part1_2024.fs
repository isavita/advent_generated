
open System
open System.IO

let lines = File.ReadAllLines "input.txt"
let empty = Array.findIndex (fun (s:string) -> s.Length = 0) lines

let grid = lines.[0..empty-1] |> Array.map (fun s -> s.ToCharArray())
let moves = String.concat "" lines.[empty+1..]

let mutable (robotR, robotC) =
    grid |> Array.mapi (fun r row ->
        match Array.tryFindIndex ((=)'@') row with
        | Some c -> Some (r,c)
        | None -> None)
    |> Array.pick id

let rows = grid.Length
let cols = grid.[0].Length

let rec push r c dr dc =
    let nr, nc = r + dr, c + dc
    if nr < 0 || nr >= rows || nc < 0 || nc >= cols then false
    else
        match grid.[nr].[nc] with
        | '#' -> false
        | 'O' ->
            if push nr nc dr dc then
                grid.[nr].[nc] <- 'O'
                true
            else false
        | '.' ->
            grid.[nr].[nc] <- 'O'
            true
        | _ -> false

for move in moves do
    let dr, dc =
        match move with
        | '^' -> -1, 0
        | 'v' -> 1, 0
        | '<' -> 0, -1
        | '>' -> 0, 1
        | _ -> 0, 0
    if dr <> 0 || dc <> 0 then
        let nr, nc = robotR + dr, robotC + dc
        if nr >= 0 && nr < rows && nc >= 0 && nc < cols then
            match grid.[nr].[nc] with
            | '#' -> ()
            | 'O' ->
                if push nr nc dr dc then
                    grid.[robotR].[robotC] <- '.'
                    grid.[nr].[nc] <- '@'
                    robotR <- nr
                    robotC <- nc
            | '.' ->
                grid.[robotR].[robotC] <- '.'
                grid.[nr].[nc] <- '@'
                robotR <- nr
                robotC <- nc
            | _ -> ()

let total =
    [| for r in 0..rows-1 do
         for c in 0..cols-1 do
             if grid.[r].[c] = 'O' then yield r * 100 + c |]
    |> Array.sum

printfn "%d" total
