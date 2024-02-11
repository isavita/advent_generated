
module Day5

let input = System.IO.File.ReadAllLines "input.txt"

let isNice1 (s:string) =
    let vowels = "aeiou"
    let mutable vowelCount = 0
    let mutable doubleLetter = false
    let mutable hasForbidden = false
    let mutable prevChar = ' '
    for c in s do
        match c with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> vowelCount <- vowelCount + 1
        | _ -> ()
        if c = prevChar then doubleLetter <- true
        match (prevChar, c) with
        | ('a', 'b') | ('c', 'd') | ('p', 'q') | ('x', 'y') -> hasForbidden <- true
        | _ -> ()
        prevChar <- c
    vowelCount >= 3 && doubleLetter && not hasForbidden

let isNice2 (s:string) =
    let mutable pairTwice = false
    let mutable repeatWithOneBetween = false
    let mutable prevPair = ""
    let mutable prevChar = ' '
    for i = 1 to s.Length - 1 do
        let pair = s.[i-1..i]
        if s.IndexOf(pair, i) <> -1 then pairTwice <- true
        if i > 1 && s.[i-2] = s.[i] then repeatWithOneBetween <- true
    pairTwice && repeatWithOneBetween

let niceStrings1 = input |> Array.filter isNice1
let niceStrings2 = input |> Array.filter isNice2

printfn "%d" (Array.length niceStrings1)
printfn "%d" (Array.length niceStrings2)
