
module Day5

open System.IO
open System.Collections.Generic

let isNice (str: string) =
    let vowels = set ['a'; 'e'; 'i'; 'o'; 'u']
    let hasThreeVowels = str |> Seq.filter (vowels.Contains) |> Seq.length >= 3
    let hasDoubleLetter = str |> Seq.pairwise |> Seq.exists (fun (a, b) -> a = b)
    let hasDisallowedSubstring = ["ab"; "cd"; "pq"; "xy"]
                                  |> List.exists (fun substring -> str.Contains(substring))
                                  |> not
    hasThreeVowels && hasDoubleLetter && hasDisallowedSubstring

let input = File.ReadAllLines "input.txt"
let result = input |> Array.filter isNice |> Array.length
printfn "Result: %d" result
