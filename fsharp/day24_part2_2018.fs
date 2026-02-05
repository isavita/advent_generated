
open System
open System.IO
open System.Text.RegularExpressions

type Group = { 
    Id: int; Army: string; mutable Units: int; HP: int; 
    AttackDamage: int; AttackType: string; Initiative: int; 
    Weaknesses: string list; Immunities: string list 
}

let ep g = g.Units * g.AttackDamage

let calcDamage a d =
    if List.contains a.AttackType d.Immunities then 0
    elif List.contains a.AttackType d.Weaknesses then (ep a) * 2
    else ep a

let parseLine id army line =
    let m = Regex.Match(line, @"(\d+) units each with (\d+) hit points (.*)with an attack that does (\d+) (\w+) damage at initiative (\d+)")
    let units = int m.Groups.[1].Value
    let hp = int m.Groups.[2].Value
    let modsStr = m.Groups.[3].Value
    let damage = int m.Groups.[4].Value
    let dType = m.Groups.[5].Value
    let initiative = int m.Groups.[6].Value
    let mutable weak = []
    let mutable immune = []
    if modsStr.Contains "(" then
        let content = modsStr.Substring(modsStr.IndexOf('(') + 1, modsStr.IndexOf(')') - modsStr.IndexOf('(') - 1)
        for part in content.Split(';') do
            let p = part.Trim()
            if p.StartsWith("weak to ") then weak <- p.Replace("weak to ", "").Split(',') |> Array.map (fun s -> s.Trim()) |> Array.toList
            elif p.StartsWith("immune to ") then immune <- p.Replace("immune to ", "").Split(',') |> Array.map (fun s -> s.Trim()) |> Array.toList
    { Id = id; Army = army; Units = units; HP = hp; AttackDamage = damage; AttackType = dType; Initiative = initiative; Weaknesses = weak; Immunities = immune }

let simulate boost (initialGroups: Group list) =
    let gs = initialGroups |> List.map (fun g -> { g with Units = g.Units; AttackDamage = if g.Army = "Immune System" then g.AttackDamage + boost else g.AttackDamage }) |> List.toArray
    let mutable stalemate = false
    let countUnits army = gs |> Array.sumBy (fun g -> if g.Army = army && g.Units > 0 then g.Units else 0)
    let mutable imUnits = countUnits "Immune System"
    let mutable inUnits = countUnits "Infection"
    while imUnits > 0 && inUnits > 0 && not stalemate do
        let attackers = gs |> Array.indexed |> Array.filter (fun (_, g) -> g.Units > 0) |> Array.sortByDescending (fun (_, g) -> (ep g, g.Initiative))
        let mutable targets = Map.empty
        let mutable targeted = Set.empty
        for idx, a in attackers do
            let potential = 
                gs |> Array.indexed |> Array.filter (fun (i, g) -> g.Army <> a.Army && g.Units > 0 && not (Set.contains i targeted) && (calcDamage a g) > 0)
                |> Array.sortByDescending (fun (_, g) -> (calcDamage a g, ep g, g.Initiative))
            if not (Array.isEmpty potential) then
                let tIdx, _ = Array.head potential
                targets <- Map.add idx tIdx targets
                targeted <- Set.add tIdx targeted
        let mutable killedAny = false
        let order = gs |> Array.indexed |> Array.filter (fun (_, g) -> g.Units > 0) |> Array.sortByDescending (fun (_, g) -> g.Initiative)
        for idx, _ in order do
            if gs.[idx].Units > 0 then
                match Map.tryFind idx targets with
                | Some tIdx ->
                    let k = min gs.[tIdx].Units ((calcDamage gs.[idx] gs.[tIdx]) / gs.[tIdx].HP)
                    if k > 0 then
                        gs.[tIdx].Units <- gs.[tIdx].Units - k
                        killedAny <- true
                | None -> ()
        if not killedAny then stalemate <- true
        imUnits <- countUnits "Immune System"
        inUnits <- countUnits "Infection"
    (imUnits > 0 && inUnits = 0, imUnits)

[<EntryPoint>]
let main _ =
    if not (File.Exists "input.txt") then 0
    else
        let lines = File.ReadAllLines "input.txt"
        let mutable army, groups, cid = "", [], 1
        for l in lines do
            if l.EndsWith ":" then army <- l.TrimEnd ':'
            elif l.Trim() <> "" then groups <- (parseLine cid army l) :: groups; cid <- cid + 1
        let init = List.rev groups
        let rec findBoost b =
            let win, units = simulate b init
            if win then units else findBoost (b + 1)
        printfn "%d" (findBoost 0)
        0
