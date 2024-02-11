
module Day3

let input = System.IO.File.ReadAllText "input.txt"

let mutable x = 0
let mutable y = 0
let mutable houses = Set.empty

houses <- houses.Add (x, y)

for i in 0 .. input.Length - 1 do
    match input.[i] with
    | '^' -> y <- y + 1
    | 'v' -> y <- y - 1
    | '>' -> x <- x + 1
    | '<' -> x <- x - 1
    | _ -> ()

    houses <- houses.Add (x, y)

printfn "%d" houses.Count
