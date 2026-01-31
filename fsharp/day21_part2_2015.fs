
open System
open System.IO

type Item = { cost:int; damage:int; armor:int }
type Character = { hitPoints:int; mutable damage:int; mutable armor:int }

let parseStat (line:string) =
    line.Split([|':'|], 2, StringSplitOptions.None).[1].Trim() |> int

let playerWins (player:Character) (boss:Character) =
    let pD = max 1 (player.damage - boss.armor)
    let bD = max 1 (boss.damage - player.armor)
    let pTurns = (boss.hitPoints + pD - 1) / pD
    let bTurns = (player.hitPoints + bD - 1) / bD
    pTurns <= bTurns

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let boss = { hitPoints = parseStat lines.[0]; damage = parseStat lines.[1]; armor = parseStat lines.[2] }

    let weapons = [|
        {cost=8;  damage=4; armor=0}
        {cost=10; damage=5; armor=0}
        {cost=25; damage=6; armor=0}
        {cost=40; damage=7; armor=0}
        {cost=74; damage=8; armor=0}
    |]

    let armors = [|
        {cost=0;   damage=0; armor=0}
        {cost=13;  damage=0; armor=1}
        {cost=31;  damage=0; armor=2}
        {cost=53;  damage=0; armor=3}
        {cost=75;  damage=0; armor=4}
        {cost=102; damage=0; armor=5}
    |]

    let rings = [|
        {cost=0;   damage=0; armor=0}
        {cost=25;  damage=1; armor=0}
        {cost=50;  damage=2; armor=0}
        {cost=100; damage=3; armor=0}
        {cost=20;  damage=0; armor=1}
        {cost=40;  damage=0; armor=2}
        {cost=80;  damage=0; armor=3}
    |]

    let maxCost =
        seq {
            for w in weapons do
                for a in armors do
                    for i = 0 to rings.Length-2 do
                        for j = i+1 to rings.Length-1 do
                            let player = { hitPoints=100; damage=w.damage + rings.[i].damage + rings.[j].damage
                                           armor=a.armor + rings.[i].armor + rings.[j].armor }
                            let cost = w.cost + a.cost + rings.[i].cost + rings.[j].cost
                            if not (playerWins player boss) then yield cost
        }
        |> Seq.max

    printfn "%d" maxCost
    0
