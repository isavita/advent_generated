
module Day2

open System.IO

let input = File.ReadAllLines "input.txt"

let keypad1 = [| [|'1'; '2'; '3'|]; [|'4'; '5'; '6'|]; [|'7'; '8'; '9'|] |]
let keypad2 = [| [|'0'; '0'; '1'; '0'; '0'|]; [|'0'; '2'; '3'; '4'; '0'|]; [|'5'; '6'; '7'; '8'; '9'|]; [|'0'; 'A'; 'B'; 'C'; '0'|]; [|'0'; '0'; 'D'; '0'; '0'|] |]

let move (keypad: char[][]) (start: char) (direction: char) =
    let row = keypad |> Array.findIndex (fun x -> Array.contains start x)
    let col = keypad.[row] |> Array.findIndex (fun x -> x = start)
    match direction with
    | 'U' -> if row > 0 && keypad.[row-1].[col] <> '0' then keypad.[row-1].[col] else start
    | 'D' -> if row < keypad.Length - 1 && keypad.[row+1].[col] <> '0' then keypad.[row+1].[col] else start
    | 'L' -> if col > 0 && keypad.[row].[col-1] <> '0' then keypad.[row].[col-1] else start
    | 'R' -> if col < keypad.[row].Length - 1 && keypad.[row].[col+1] <> '0' then keypad.[row].[col+1] else start
    | _ -> start

let getCode (keypad: char[][]) (instructions: string[]) =
    let mutable code = ""
    let mutable current = '5'
    for instruction in instructions do
        for direction in instruction do
            current <- move keypad current direction
        code <- code + string current
    code

let result1 = getCode keypad1 input
let result2 = getCode keypad2 input

printfn "%s" result1
printfn "%s" result2
