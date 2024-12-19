
import Foundation

struct Blueprint {
    let id: Int
    let oreForOreRobot: Int
    let oreForClayRobot: Int
    let oreForObsidianRobot: Int
    let clayForObsidianRobot: Int
    let oreForGeodeRobot: Int
    let obsidianForGeodeRobot: Int
}

struct State {
    let blueprint: Blueprint
    var ore: Int
    var clay: Int
    var obsidian: Int
    var geode: Int
    var oreRobots: Int
    var clayRobots: Int
    var obsidianRobots: Int
    var geodeRobots: Int
}

func parseInput(input: String) -> [Blueprint] {
    var blueprints: [Blueprint] = []
    let lines = input.components(separatedBy: "\n")
    for line in lines {
        let components = line.components(separatedBy: ": ")
        let idString = components[0].replacingOccurrences(of: "Blueprint ", with: "")
        let id = Int(idString)!
        let costs = components[1].components(separatedBy: ". ")
        
        var oreForOreRobot = 0
        var oreForClayRobot = 0
        var oreForObsidianRobot = 0
        var clayForObsidianRobot = 0
        var oreForGeodeRobot = 0
        var obsidianForGeodeRobot = 0
        
        for cost in costs {
            let words = cost.components(separatedBy: " ")
            if cost.hasPrefix("Each ore robot") {
                oreForOreRobot = Int(words[4])!
            } else if cost.hasPrefix("Each clay robot") {
                oreForClayRobot = Int(words[4])!
            } else if cost.hasPrefix("Each obsidian robot") {
                oreForObsidianRobot = Int(words[4])!
                clayForObsidianRobot = Int(words[7])!
            } else if cost.hasPrefix("Each geode robot") {
                oreForGeodeRobot = Int(words[4])!
                obsidianForGeodeRobot = Int(words[7])!
            }
        }
        
        blueprints.append(Blueprint(id: id, oreForOreRobot: oreForOreRobot, oreForClayRobot: oreForClayRobot, oreForObsidianRobot: oreForObsidianRobot, clayForObsidianRobot: clayForObsidianRobot, oreForGeodeRobot: oreForGeodeRobot, obsidianForGeodeRobot: obsidianForGeodeRobot))
    }
    return blueprints
}

func newState(blueprint: Blueprint) -> State {
    return State(blueprint: blueprint, ore: 0, clay: 0, obsidian: 0, geode: 0, oreRobots: 1, clayRobots: 0, obsidianRobots: 0, geodeRobots: 0)
}

extension State {
    mutating func farm() {
        ore += oreRobots
        clay += clayRobots
        obsidian += obsidianRobots
        geode += geodeRobots
    }
    
    func hash(time: Int) -> String {
        return "\(time),\(ore),\(clay),\(obsidian),\(geode),\(oreRobots),\(clayRobots),\(obsidianRobots),\(geodeRobots)"
    }
    
    func copy() -> State {
        return self
    }
    
    func calcMostGeodes(time: Int, memo: inout [String: Int], totalTime: Int, earliestGeode: inout Int) -> Int {
        if time == totalTime {
            return geode
        }
        
        let h = hash(time: time)
        if let v = memo[h] {
            return v
        }
        
        if geode == 0 && time > earliestGeode {
            return 0
        }
        
        var mostGeodes = geode
        
        if ore >= blueprint.oreForGeodeRobot && obsidian >= blueprint.obsidianForGeodeRobot {
            var cp = copy()
            cp.farm()
            cp.ore -= cp.blueprint.oreForGeodeRobot
            cp.obsidian -= cp.blueprint.obsidianForGeodeRobot
            cp.geodeRobots += 1
            if cp.geodeRobots == 1 {
                earliestGeode = min(earliestGeode, time + 1)
            }
            mostGeodes = max(mostGeodes, cp.calcMostGeodes(time: time + 1, memo: &memo, totalTime: totalTime, earliestGeode: &earliestGeode))
            
            memo[h] = mostGeodes
            return mostGeodes
        }
        
        if time <= totalTime - 16 && oreRobots < blueprint.oreForObsidianRobot * 2 && ore >= blueprint.oreForOreRobot {
            var cp = copy()
            cp.ore -= cp.blueprint.oreForOreRobot
            cp.farm()
            cp.oreRobots += 1
            mostGeodes = max(mostGeodes, cp.calcMostGeodes(time: time + 1, memo: &memo, totalTime: totalTime, earliestGeode: &earliestGeode))
        }
        
        if time <= totalTime - 8 && clayRobots < blueprint.clayForObsidianRobot && ore >= blueprint.oreForClayRobot {
            var cp = copy()
            cp.ore -= cp.blueprint.oreForClayRobot
            cp.farm()
            cp.clayRobots += 1
            mostGeodes = max(mostGeodes, cp.calcMostGeodes(time: time + 1, memo: &memo, totalTime: totalTime, earliestGeode: &earliestGeode))
        }
        
        if time <= totalTime - 4 && obsidianRobots < blueprint.obsidianForGeodeRobot && ore >= blueprint.oreForObsidianRobot && clay >= blueprint.clayForObsidianRobot {
            var cp = copy()
            cp.ore -= cp.blueprint.oreForObsidianRobot
            cp.clay -= cp.blueprint.clayForObsidianRobot
            cp.farm()
            cp.obsidianRobots += 1
            mostGeodes = max(mostGeodes, cp.calcMostGeodes(time: time + 1, memo: &memo, totalTime: totalTime, earliestGeode: &earliestGeode))
        }
        
        var cp = copy()
        cp.farm()
        mostGeodes = max(mostGeodes, cp.calcMostGeodes(time: time + 1, memo: &memo, totalTime: totalTime, earliestGeode: &earliestGeode))
        
        memo[h] = mostGeodes
        return mostGeodes
    }
}

func part1(input: String) -> Int {
    let blueprints = parseInput(input: input)
    var sum = 0
    for bp in blueprints {
        var st = newState(blueprint: bp)
        var memo: [String: Int] = [:]
        var earliestGeode = 24
        let geodesMade = st.calcMostGeodes(time: 0, memo: &memo, totalTime: 24, earliestGeode: &earliestGeode)
        sum += st.blueprint.id * geodesMade
    }
    return sum
}

let input = try! String(contentsOfFile: "input.txt").trimmingCharacters(in: .whitespacesAndNewlines)
print(part1(input: input))
