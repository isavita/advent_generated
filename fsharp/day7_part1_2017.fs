
module Day7

open System
open System.Text.RegularExpressions
open System.IO

let lines = File.ReadAllLines("input.txt")

let holderMap = new System.Collections.Generic.Dictionary<string,bool>()
let heldMap = new System.Collections.Generic.Dictionary<string,bool>()

let re = new Regex("[a-z]+")

for line in lines do
    let names = re.Matches(line) |> Seq.cast<Match> |> Seq.map(fun m -> m.Value)
    let holder = names |> Seq.head
    holderMap.[holder] <- true

    if Seq.length names > 1 then
        for i in 1..Seq.length names - 1 do
            heldMap.[names |> Seq.item i] <- true

for holder in holderMap.Keys do
    if not (heldMap.ContainsKey(holder)) then
        Console.WriteLine(holder)
        Environment.Exit(0)
