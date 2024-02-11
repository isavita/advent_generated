module Day19

open System
open System.IO
open System.Text.RegularExpressions

let rec findCombinations (containers: int list) target index count minCount ways =
    if target = 0 then
        if !minCount = 0 || count < !minCount then
            minCount := count
            ways := 1
        elif count = !minCount then
            ways := !ways + 1
    elif target < 0 || index >= containers.Length then
        ()
    else
        findCombinations containers (target - containers.[index]) (index + 1) (count + 1) minCount ways
        findCombinations containers target (index + 1) count minCount ways

let mutable minCount = ref 0
let mutable ways = ref 0

let file = File.ReadAllLines("input.txt")
let containers = file |> Array.map int

findCombinations (containers |> Array.toList) 150 0 0 minCount ways
Console.WriteLine(!ways)