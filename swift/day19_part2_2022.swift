
import Foundation

struct Blueprint {
    let id: Int
    let oreRobotCost: Int
    let clayRobotCost: Int
    let obsidianRobotOreCost: Int
    let obsidianRobotClayCost: Int
    let geodeRobotOreCost: Int
    let geodeRobotObsidianCost: Int
}

struct State: Hashable {
    let time: Int
    let oreRobots: Int
    let clayRobots: Int
    let obsidianRobots: Int
    let geodeRobots: Int
    let ore: Int
    let clay: Int
    let obsidian: Int
    let geodes: Int
}

func parseBlueprint(from line: String) -> Blueprint {
    let scanner = Scanner(string: line)
    var id = 0
    var oreRobotCost = 0
    var clayRobotCost = 0
    var obsidianRobotOreCost = 0
    var obsidianRobotClayCost = 0
    var geodeRobotOreCost = 0
    var geodeRobotObsidianCost = 0

    scanner.scanString("Blueprint", into: nil)
    scanner.scanInt(&id)
    scanner.scanString(":", into: nil)
    scanner.scanString("Each ore robot costs", into: nil)
    scanner.scanInt(&oreRobotCost)
    scanner.scanString("ore.", into: nil)
    scanner.scanString("Each clay robot costs", into: nil)
    scanner.scanInt(&clayRobotCost)
    scanner.scanString("ore.", into: nil)
    scanner.scanString("Each obsidian robot costs", into: nil)
    scanner.scanInt(&obsidianRobotOreCost)
    scanner.scanString("ore and", into: nil)
    scanner.scanInt(&obsidianRobotClayCost)
    scanner.scanString("clay.", into: nil)
    scanner.scanString("Each geode robot costs", into: nil)
    scanner.scanInt(&geodeRobotOreCost)
    scanner.scanString("ore and", into: nil)
    scanner.scanInt(&geodeRobotObsidianCost)
    scanner.scanString("obsidian.", into: nil)
    
    return Blueprint(id: id, oreRobotCost: oreRobotCost, clayRobotCost: clayRobotCost,
                     obsidianRobotOreCost: obsidianRobotOreCost, obsidianRobotClayCost: obsidianRobotClayCost,
                     geodeRobotOreCost: geodeRobotOreCost, geodeRobotObsidianCost: geodeRobotObsidianCost)
}

func maxGeodes(blueprint: Blueprint, timeLimit: Int) -> Int {
    var maxGeodesFound = 0
    var queue: [State] = []
    var visited: Set<State> = []
    
    let initialState = State(time: 0, oreRobots: 1, clayRobots: 0, obsidianRobots: 0, geodeRobots: 0,
                             ore: 0, clay: 0, obsidian: 0, geodes: 0)
    queue.append(initialState)
    visited.insert(initialState)
    
    while !queue.isEmpty {
        let currentState = queue.removeFirst()
        
        if currentState.time == timeLimit {
            maxGeodesFound = max(maxGeodesFound, currentState.geodes)
            continue
        }
        
        let newOre = currentState.ore + currentState.oreRobots
        let newClay = currentState.clay + currentState.clayRobots
        let newObsidian = currentState.obsidian + currentState.obsidianRobots
        let newGeodes = currentState.geodes + currentState.geodeRobots
        
        // Optimization: Calculate a theoretical maximum number of geodes and prune if current state cannot beat the current max
        let remainingTime = timeLimit - currentState.time
        let potentialGeodes = newGeodes + currentState.geodeRobots * remainingTime + (remainingTime * (remainingTime - 1)) / 2
        if potentialGeodes < maxGeodesFound {
          continue
        }
      
        // Option 1: Do nothing (always an option)
        let nextStateDoNothing = State(time: currentState.time + 1, oreRobots: currentState.oreRobots, clayRobots: currentState.clayRobots,
                                       obsidianRobots: currentState.obsidianRobots, geodeRobots: currentState.geodeRobots,
                                       ore: newOre, clay: newClay, obsidian: newObsidian, geodes: newGeodes)
        if !visited.contains(nextStateDoNothing) {
            queue.append(nextStateDoNothing)
            visited.insert(nextStateDoNothing)
        }
       
        // Option 2: Build an ore robot
        if currentState.ore >= blueprint.oreRobotCost {
          let nextStateOreRobot = State(time: currentState.time + 1, oreRobots: currentState.oreRobots + 1, clayRobots: currentState.clayRobots,
                                           obsidianRobots: currentState.obsidianRobots, geodeRobots: currentState.geodeRobots,
                                           ore: newOre - blueprint.oreRobotCost, clay: newClay, obsidian: newObsidian, geodes: newGeodes)
            
          if !visited.contains(nextStateOreRobot) && nextStateOreRobot.oreRobots <= max(blueprint.oreRobotCost, blueprint.clayRobotCost, blueprint.obsidianRobotOreCost, blueprint.geodeRobotOreCost) {
                queue.append(nextStateOreRobot)
                visited.insert(nextStateOreRobot)
            }
        }
        
        // Option 3: Build a clay robot
        if currentState.ore >= blueprint.clayRobotCost {
            let nextStateClayRobot = State(time: currentState.time + 1, oreRobots: currentState.oreRobots, clayRobots: currentState.clayRobots + 1,
                                            obsidianRobots: currentState.obsidianRobots, geodeRobots: currentState.geodeRobots,
                                            ore: newOre - blueprint.clayRobotCost, clay: newClay, obsidian: newObsidian, geodes: newGeodes)
            if !visited.contains(nextStateClayRobot) && nextStateClayRobot.clayRobots <= blueprint.obsidianRobotClayCost {
                queue.append(nextStateClayRobot)
                visited.insert(nextStateClayRobot)
            }
        }
        
        // Option 4: Build an obsidian robot
        if currentState.ore >= blueprint.obsidianRobotOreCost && currentState.clay >= blueprint.obsidianRobotClayCost {
            let nextStateObsidianRobot = State(time: currentState.time + 1, oreRobots: currentState.oreRobots, clayRobots: currentState.clayRobots,
                                                obsidianRobots: currentState.obsidianRobots + 1, geodeRobots: currentState.geodeRobots,
                                                ore: newOre - blueprint.obsidianRobotOreCost, clay: newClay - blueprint.obsidianRobotClayCost,
                                                obsidian: newObsidian, geodes: newGeodes)
            if !visited.contains(nextStateObsidianRobot) && nextStateObsidianRobot.obsidianRobots <= blueprint.geodeRobotObsidianCost {
                queue.append(nextStateObsidianRobot)
                visited.insert(nextStateObsidianRobot)
            }
        }
        
        // Option 5: Build a geode robot
        if currentState.ore >= blueprint.geodeRobotOreCost && currentState.obsidian >= blueprint.geodeRobotObsidianCost {
            let nextStateGeodeRobot = State(time: currentState.time + 1, oreRobots: currentState.oreRobots, clayRobots: currentState.clayRobots,
                                             obsidianRobots: currentState.obsidianRobots, geodeRobots: currentState.geodeRobots + 1,
                                             ore: newOre - blueprint.geodeRobotOreCost, clay: newClay, obsidian: newObsidian - blueprint.geodeRobotObsidianCost,
                                             geodes: newGeodes)
            if !visited.contains(nextStateGeodeRobot) {
                queue.append(nextStateGeodeRobot)
                visited.insert(nextStateGeodeRobot)
            }
        }
          queue.sort { (state1, state2) -> Bool in
              if state1.geodes != state2.geodes {
                  return state1.geodes > state2.geodes
              } else if state1.geodeRobots != state2.geodeRobots {
                  return state1.geodeRobots > state2.geodeRobots
              }
             else if state1.obsidianRobots != state2.obsidianRobots{
                  return state1.obsidianRobots > state2.obsidianRobots
             } else if state1.clayRobots != state2.clayRobots{
                 return state1.clayRobots > state2.clayRobots
             }
             else {
                  return state1.oreRobots > state2.oreRobots
              }
            
        }
    }
    
    return maxGeodesFound
}

func main() {
    guard let input = try? String(contentsOfFile: "input.txt") else {
        print("Error reading input file")
        return
    }

    let lines = input.trimmingCharacters(in: .whitespacesAndNewlines).components(separatedBy: .newlines)
    let blueprints = lines.map { parseBlueprint(from: $0) }

    // Part 1
    var totalQualityLevel = 0
    for blueprint in blueprints {
        let maxGeodesForBlueprint = maxGeodes(blueprint: blueprint, timeLimit: 24)
        totalQualityLevel += blueprint.id * maxGeodesForBlueprint
    }
    print("Total quality level (Part 1): \(totalQualityLevel)")

    // Part 2
    var productOfMaxGeodes = 1
    for i in 0..<min(3, blueprints.count) {  //first three blueprints
        let maxGeodesForBlueprint = maxGeodes(blueprint: blueprints[i], timeLimit: 32)
        productOfMaxGeodes *= maxGeodesForBlueprint
    }
    print("Product of max geodes for first three blueprints (Part 2): \(productOfMaxGeodes)")
}

main()
