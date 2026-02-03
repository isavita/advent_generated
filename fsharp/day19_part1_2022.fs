
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

type Blueprint =
    { Id: int
      OreForOreRobot: int
      OreForClayRobot: int
      OreForObsidianRobot: int
      ClayForObsidianRobot: int
      OreForGeodeRobot: int
      ObsidianForGeodeRobot: int }

type State(bp: Blueprint) =
    member val Blueprint = bp with get
    member val Ore = 0 with get, set
    member val Clay = 0 with get, set
    member val Obsidian = 0 with get, set
    member val Geode = 0 with get, set
    member val OreRobots = 1 with get, set
    member val ClayRobots = 0 with get, set
    member val ObsidianRobots = 0 with get, set
    member val GeodeRobots = 0 with get, set
    member this.Farm() =
        this.Ore <- this.Ore + this.OreRobots
        this.Clay <- this.Clay + this.ClayRobots
        this.Obsidian <- this.Obsidian + this.ObsidianRobots
        this.Geode <- this.Geode + this.GeodeRobots
    member this.Hash(time: int) =
        $"{time},{this.Ore},{this.Clay},{this.Obsidian},{this.Geode},{this.OreRobots},{this.ClayRobots},{this.ObsidianRobots},{this.GeodeRobots}"
    member this.Copy() =
        let c = State(this.Blueprint)
        c.Ore <- this.Ore
        c.Clay <- this.Clay
        c.Obsidian <- this.Obsidian
        c.Geode <- this.Geode
        c.OreRobots <- this.OreRobots
        c.ClayRobots <- this.ClayRobots
        c.ObsidianRobots <- this.ObsidianRobots
        c.GeodeRobots <- this.GeodeRobots
        c

    member this.CalcMostGeodes(time: int, memo: Dictionary<string,int>, totalTime: int, earliestGeode: int) : int =
        if time = totalTime then this.Geode
        else
            let key = this.Hash(time)
            match memo.TryGetValue(key) with
            | true, v -> v
            | _ ->
                if this.Geode = 0 && time > earliestGeode then
                    0
                else
                    let mutable best = this.Geode
                    if this.Ore >= this.Blueprint.OreForGeodeRobot && this.Obsidian >= this.Blueprint.ObsidianForGeodeRobot then
                        let cp = this.Copy()
                        cp.Farm()
                        cp.Ore <- cp.Ore - this.Blueprint.OreForGeodeRobot
                        cp.Obsidian <- cp.Obsidian - this.Blueprint.ObsidianForGeodeRobot
                        cp.GeodeRobots <- cp.GeodeRobots + 1
                        let newEarliest = if cp.GeodeRobots = 1 then min earliestGeode (time + 1) else earliestGeode
                        best <- max best (cp.CalcMostGeodes(time + 1, memo, totalTime, newEarliest))
                        memo.[key] <- best
                        best
                    else
                        if time <= totalTime - 16 && this.OreRobots < this.Blueprint.OreForObsidianRobot * 2 && this.Ore >= this.Blueprint.OreForOreRobot then
                            let cp = this.Copy()
                            cp.Ore <- cp.Ore - this.Blueprint.OreForOreRobot
                            cp.Farm()
                            cp.OreRobots <- cp.OreRobots + 1
                            best <- max best (cp.CalcMostGeodes(time + 1, memo, totalTime, earliestGeode))
                        if time <= totalTime - 8 && this.ClayRobots < this.Blueprint.ClayForObsidianRobot && this.Ore >= this.Blueprint.OreForClayRobot then
                            let cp = this.Copy()
                            cp.Ore <- cp.Ore - this.Blueprint.OreForClayRobot
                            cp.Farm()
                            cp.ClayRobots <- cp.ClayRobots + 1
                            best <- max best (cp.CalcMostGeodes(time + 1, memo, totalTime, earliestGeode))
                        if time <= totalTime - 4 && this.ObsidianRobots < this.Blueprint.ObsidianForGeodeRobot && this.Ore >= this.Blueprint.OreForObsidianRobot && this.Clay >= this.Blueprint.ClayForObsidianRobot then
                            let cp = this.Copy()
                            cp.Ore <- cp.Ore - this.Blueprint.OreForObsidianRobot
                            cp.Clay <- cp.Clay - this.Blueprint.ClayForObsidianRobot
                            cp.Farm()
                            cp.ObsidianRobots <- cp.ObsidianRobots + 1
                            best <- max best (cp.CalcMostGeodes(time + 1, memo, totalTime, earliestGeode))
                        let cp = this.Copy()
                        cp.Farm()
                        best <- max best (cp.CalcMostGeodes(time + 1, memo, totalTime, earliestGeode))
                        memo.[key] <- best
                        best

let parseInput (input: string) =
    let pattern = Regex(@"Blueprint (\d+): Each ore robot costs (\d+) ore\. Each clay robot costs (\d+) ore\. Each obsidian robot costs (\d+) ore and (\d+) clay\. Each geode robot costs (\d+) ore and (\d+) obsidian\.")
    input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line ->
        let m = pattern.Match(line)
        { Id = int m.Groups.[1].Value
          OreForOreRobot = int m.Groups.[2].Value
          OreForClayRobot = int m.Groups.[3].Value
          OreForObsidianRobot = int m.Groups.[4].Value
          ClayForObsidianRobot = int m.Groups.[5].Value
          OreForGeodeRobot = int m.Groups.[6].Value
          ObsidianForGeodeRobot = int m.Groups.[7].Value })
    |> List.ofArray

let part1 (input: string) =
    let bps = parseInput input
    bps
    |> List.sumBy (fun bp ->
        let st = State(bp)
        let memo = Dictionary<string,int>()
        let geodes = st.CalcMostGeodes(0, memo, 24, 24)
        bp.Id * geodes)

[<EntryPoint>]
let main _ =
    let input = File.ReadAllText("input.txt").Trim()
    printfn "%d" (part1 input)
    0
