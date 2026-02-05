open System
open System.IO
open System.Text.RegularExpressions

type Group =
    {
        mutable Id: int
        mutable ArmyId: int
        mutable Units: int
        HitPoints: int
        AttackDamage: int
        AttackType: string
        Initiative: int
        Immunities: string list
        Weaknesses: string list
        mutable Target: Group option
        mutable Attacker: Group option
    }
    with
        member this.EffectivePower = this.Units * this.AttackDamage

let parseInput filename =
    let lines = File.ReadAllLines(filename)
    let groups = new System.Collections.Generic.List<Group>()
    let mutable groupId = 0
    let mutable armyId = 0
    let pattern = Regex(@"(\d+) units each with (\d+) hit points (?:\(([^\)]+)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)")
    for line in lines do
        if line.StartsWith("Immune System:") then armyId <- 1
        elif line.StartsWith("Infection:") then armyId <- 2
        elif not (String.IsNullOrWhiteSpace(line)) then
            let m = pattern.Match(line)
            if m.Success then
                let modifiers = m.Groups.[3].Value
                let mutable immunities = []
                let mutable weaknesses = []
                if not (String.IsNullOrWhiteSpace(modifiers)) then
                    for modLine in modifiers.Split(';') do
                        let t = modLine.Trim()
                        if t.StartsWith("immune to") then
                            let items = t.Substring(9).Trim().Split(',') |> Array.map (fun s -> s.Trim()) |> List.ofArray
                            immunities <- immunities @ items
                        elif t.StartsWith("weak to") then
                            let items = t.Substring(7).Trim().Split(',') |> Array.map (fun s -> s.Trim()) |> List.ofArray
                            weaknesses <- weaknesses @ items
                groupId <- groupId + 1
                let g =
                    {
                        Id = groupId
                        ArmyId = armyId
                        Units = int m.Groups.[1].Value
                        HitPoints = int m.Groups.[2].Value
                        AttackDamage = int m.Groups.[4].Value
                        AttackType = m.Groups.[5].Value
                        Initiative = int m.Groups.[6].Value
                        Immunities = immunities
                        Weaknesses = weaknesses
                        Target = None
                        Attacker = None
                    }
                groups.Add(g)
    groups |> Seq.toList

let CalculateDamage (attacker: Group) (defender: Group) =
    if attacker.Units <= 0 || defender.Units <= 0 then 0
    elif List.exists ((=) attacker.AttackType) defender.Immunities then 0
    else
        let damage = attacker.Units * attacker.AttackDamage
        if List.exists ((=) attacker.AttackType) defender.Weaknesses then damage * 2 else damage

let rec simulate (groups: Group list) : int =
    let immuneAlive = groups |> List.filter (fun g -> g.ArmyId = 1 && g.Units > 0) |> List.sumBy (fun g -> g.Units)
    let infectionAlive = groups |> List.filter (fun g -> g.ArmyId = 2 && g.Units > 0) |> List.sumBy (fun g -> g.Units)
    if immuneAlive = 0 || infectionAlive = 0 then immuneAlive + infectionAlive
    else
        for g in groups do g.Target <- None; g.Attacker <- None
        let activeGroups =
            groups
            |> List.filter (fun g -> g.Units > 0)
            |> List.sortWith (fun a b ->
                let cmpEp = compare b.EffectivePower a.EffectivePower
                if cmpEp <> 0 then cmpEp
                else compare b.Initiative a.Initiative
            )
        for attacker in activeGroups do
            let potentialTargets =
                activeGroups |> List.filter (fun g -> g.ArmyId <> attacker.ArmyId && g.Attacker = None)
            let sortedTargets =
                potentialTargets
                |> List.sortWith (fun x y ->
                    let dx = CalculateDamage attacker x
                    let dy = CalculateDamage attacker y
                    if dy <> dx then compare dy dx
                    else
                        let epY = y.EffectivePower
                        let epX = x.EffectivePower
                        if epY <> epX then compare epY epX
                        else compare y.Initiative x.Initiative
                )
            match sortedTargets with
            | t::_ -> attacker.Target <- Some t; t.Attacker <- Some attacker
            | [] -> ()
        let initiativeOrder =
            activeGroups |> List.sortWith (fun a b -> compare b.Initiative a.Initiative)
        let mutable totalKills = 0
        for attacker in initiativeOrder do
            match attacker.Target with
            | Some t when attacker.Units > 0 && t.Units > 0 ->
                let damage = CalculateDamage attacker t
                let unitsKilled = min t.Units (damage / t.HitPoints)
                t.Units <- t.Units - unitsKilled
                totalKills <- totalKills + unitsKilled
            | _ -> ()
        if totalKills = 0 then 0
        else simulate groups

[<EntryPoint>]
let main argv =
    let groups = parseInput "input.txt"
    let result = simulate groups
    printfn "%d" result
    0