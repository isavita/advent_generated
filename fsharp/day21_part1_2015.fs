
open System
open System.IO

type Item = { Cost:int; Damage:int; Armor:int }
type Character = { HitPoints:int; Damage:int; Armor:int }

let parseStat (line:string) = line.Split(':').[1].Trim() |> int
let max a b = if a > b then a else b
let playerWins player boss =
    let pD = max 1 (player.Damage - boss.Armor)
    let bD = max 1 (boss.Damage - player.Armor)
    let pTurns = (boss.HitPoints + pD - 1) / pD
    let bTurns = (player.HitPoints + bD - 1) / bD
    pTurns <= bTurns

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let boss = { HitPoints = parseStat lines.[0]; Damage = parseStat lines.[1]; Armor = parseStat lines.[2] }

    let weapons = [| {Cost=8; Damage=4; Armor=0}; {Cost=10; Damage=5; Armor=0}; {Cost=25; Damage=6; Armor=0}; {Cost=40; Damage=7; Armor=0}; {Cost=74; Damage=8; Armor=0} |]
    let armors  = [| {Cost=0; Damage=0; Armor=0}; {Cost=13; Damage=0; Armor=1}; {Cost=31; Damage=0; Armor=2}; {Cost=53; Damage=0; Armor=3}; {Cost=75; Damage=0; Armor=4}; {Cost=102; Damage=0; Armor=5} |]
    let rings   = [| {Cost=0; Damage=0; Armor=0}; {Cost=25; Damage=1; Armor=0}; {Cost=50; Damage=2; Armor=0}; {Cost=100; Damage=3; Armor=0}; {Cost=20; Damage=0; Armor=1}; {Cost=40; Damage=0; Armor=2}; {Cost=80; Damage=0; Armor=3} |]

    let ringPairs =
        seq {
            for i = 0 to rings.Length-1 do
                for j = i+1 to rings.Length-1 do
                    yield (rings.[i], rings.[j])
        }

    let minCost =
        seq {
            for w in weapons do
                for a in armors do
                    for (r1, r2) in ringPairs do
                        let player = { HitPoints = 100; Damage = w.Damage + r1.Damage + r2.Damage; Armor = a.Armor + r1.Armor + r2.Armor }
                        let cost = w.Cost + a.Cost + r1.Cost + r2.Cost
                        if playerWins player boss then yield cost
        } |> Seq.min

    printfn "%d" minCost
    0
