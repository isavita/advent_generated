
module Day17

let input = System.IO.File.ReadAllLines "input.txt" |> Array.map int |> Array.toList
let rec combinations sum lst =
    match lst with
    | [] -> if sum = 0 then 1 else 0
    | h::t -> combinations (sum - h) t + combinations sum t

let result = combinations 150 input
printfn "%d" result
