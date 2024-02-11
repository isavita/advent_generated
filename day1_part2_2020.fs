module Day1

let input = System.IO.File.ReadAllLines("input.txt") |> Array.map int

let rec findPair nums target = 
    match nums with
    | x::xs -> if List.exists (fun y -> x + y = target) xs then x * (List.find (fun y -> x + y = target) xs) else findPair xs target
    | [] -> 0

let rec findTriple nums target = 
    match nums with
    | x::xs -> 
        match findPair xs (target - x) with
        | 0 -> findTriple xs target
        | y -> x * y
    | [] -> 0

let part1 = findPair (Array.toList input) 2020
let part2 = findTriple (Array.toList input) 2020

printfn "%d" part1
printfn "%d" part2