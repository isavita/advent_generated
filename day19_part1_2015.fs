
module Day19

open System.IO

let input = File.ReadAllLines "input.txt"

let replacements = input.[..input.Length-3]
let molecule = input.[input.Length-1]

let mutable distinctMolecules = Set.empty<string>

for line in replacements do
    let parts = line.Split [|' '|]
    let from = parts.[0]
    let ``to`` = parts.[2] // Use backticks to escape reserved keyword 'to'
    let mutable index = 0
    while index < molecule.Length do
        let replaceIndex = molecule.IndexOf(from, index)
        if replaceIndex = -1 then
            index <- molecule.Length
        else
            distinctMolecules <- distinctMolecules.Add(molecule.Substring(0, replaceIndex) + ``to`` + molecule.Substring(replaceIndex + from.Length)) // Use backticks to escape reserved keyword 'to'
            index <- replaceIndex + 1

printfn "%d" distinctMolecules.Count
