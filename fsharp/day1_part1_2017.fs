
module Day1

let input = System.IO.File.ReadAllText "input.txt"

let sumMatchingDigits (input:string) =
    let rec loop acc prev = function
        | [] -> acc
        | h::t when h = prev -> loop (acc + (int h - int '0')) h t
        | h::t -> loop acc h t
    loop 0 input.[input.Length-1] (List.ofSeq input)

let result = sumMatchingDigits input
printfn "%d" result
