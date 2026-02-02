
open System
open System.IO
open System.Collections.Generic

let readLines (filePath: string) = File.ReadAllLines(filePath)

let isDigit (c: char) = Char.IsDigit c

let parseInt (s: string) = Int64.Parse s

let getAdjacentNumbers (grid: char[][]) (i: int) (j: int) =
    let adjNumbers = HashSet<Int64>()
    for x in max 0 (i - 1) .. min (Array.length grid - 1) (i + 1) do
        for y in max 0 (j - 1) .. min (Array.length grid.[0] - 1) (j + 1) do
            if isDigit grid.[x].[y] then
                let start = 
                    let rec loop y =
                        if y = 0 || not (isDigit grid.[x].[y - 1]) then y
                        else loop (y - 1)
                    loop y
                let end' = 
                    let rec loop y =
                        if y = Array.length grid.[0] - 1 || not (isDigit grid.[x].[y + 1]) then y
                        else loop (y + 1)
                    loop y
                let num = parseInt (new string(grid.[x], start, end' - start + 1))
                adjNumbers.Add(num) |> ignore
                for k in start .. end' do grid.[x].[k] <- '.'
    adjNumbers

let isPartNumber (grid: char[][]) (i: int) (start: int) (end': int) =
    let mutable isPart = false
    for x in max 0 (i - 1) .. min (Array.length grid - 1) (i + 1) do
        for y in max 0 (start - 1) .. min (Array.length grid.[0] - 1) (end' + 1) do
            if not (isDigit grid.[x].[y]) && grid.[x].[y] <> '.' then
                isPart <- true

    isPart

let main () =
    let lines = readLines "input.txt"
    let rows = Array.length lines
    let cols = String.length lines.[0]
    let grid = Array.map (fun (s: string) -> s.ToCharArray()) lines
    let gearGrid = Array.map Array.copy grid

    let mutable sumGear = 0L
    let mutable sumPart = 0L

    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            if gearGrid.[i].[j] = '*' then
                let adjNumbers = getAdjacentNumbers gearGrid i j
                if adjNumbers.Count = 2 then
                    let nums = Seq.toArray adjNumbers
                    sumGear <- sumGear + nums.[0] * nums.[1]

    for i in 0 .. rows - 1 do
        let mutable j = 0
        while j < cols do
            if isDigit grid.[i].[j] then
                let start = 
                    let rec loop j =
                        if j = 0 || not (isDigit grid.[i].[j - 1]) then j
                        else loop (j - 1)
                    loop j
                let end' = 
                    let rec loop j =
                        if j = cols - 1 || not (isDigit grid.[i].[j + 1]) then j
                        else loop (j + 1)
                    loop j
                if isPartNumber grid i start end' then
                    sumPart <- sumPart + parseInt (new string(grid.[i], start, end' - start + 1))
                j <- end' + 1
            else
                j <- j + 1

    printfn "Sum of part numbers: %d" sumPart
    printfn "Sum of gear ratios: %d" sumGear

main ()
