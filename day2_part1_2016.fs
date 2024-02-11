
module Day2

open System.IO

let input = File.ReadAllLines "input.txt"

let keypad = [| [|'1'; '2'; '3'|]; [|'4'; '5'; '6'|]; [|'7'; '8'; '9'|] |]

let mutable x = 1
let mutable y = 1

for line in input do
    for c in line do
        match c with
        | 'U' -> if y > 0 then y <- y - 1
        | 'D' -> if y < 2 then y <- y + 1
        | 'L' -> if x > 0 then x <- x - 1
        | 'R' -> if x < 2 then x <- x + 1
    printf "%c" keypad.[y].[x]
