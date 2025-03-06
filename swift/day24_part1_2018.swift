
import Foundation

struct Group {
    let id: Int
    var units: Int
    let hitPoints: Int
    let attackDamage: Int
    let attackType: String
    let initiative: Int
    let weaknesses: [String]
    let immunities: [String]
    let army: String

    var effectivePower: Int {
        return units * attackDamage
    }

    func damageDealt(to defender: Group) -> Int {
        if defender.immunities.contains(attackType) {
            return 0
        } else if defender.weaknesses.contains(attackType) {
            return effectivePower * 2
        } else {
            return effectivePower
        }
    }
}

func parseInput(_ input: String) -> [Group] {
    var groups = [Group]()
    var currentArmy: String?
    var id = 0

    for line in input.components(separatedBy: .newlines) {
        if line.isEmpty { continue }

        if line.hasPrefix("Immune System:") {
            currentArmy = "Immune System"
            continue
        } else if line.hasPrefix("Infection:") {
            currentArmy = "Infection"
            continue
        }
        
        guard let army = currentArmy else { fatalError("Parse error: army not set") }

        let parts = line.components(separatedBy: " ")
        let units = Int(parts[0])!
        let hitPoints = Int(parts[4])!
        
        var weaknesses: [String] = []
        var immunities: [String] = []
        
        if let weaknessesImmunitiesStart = line.firstIndex(of: "(") {
            let weaknessesImmunitiesEnd = line.firstIndex(of: ")")!
            let weaknessesImmunitiesString = line[weaknessesImmunitiesStart...weaknessesImmunitiesEnd]
            let weaknessesImmunitiesParts = weaknessesImmunitiesString.dropFirst().dropLast().components(separatedBy: "; ")
            
            for part in weaknessesImmunitiesParts {
                if part.hasPrefix("weak to ") {
                    weaknesses = part.dropFirst(8).components(separatedBy: ", ")
                } else if part.hasPrefix("immune to ") {
                    immunities = part.dropFirst(10).components(separatedBy: ", ")
                }
            }
            
        }

        let attackDamage = Int(parts[parts.count - 6])!
        let attackType = parts[parts.count - 5]
        let initiative = Int(parts[parts.count - 1])!

        id += 1
        let group = Group(id: id, units: units, hitPoints: hitPoints, attackDamage: attackDamage, attackType: attackType, initiative: initiative, weaknesses: weaknesses, immunities: immunities, army: army)
        groups.append(group)
    }

    return groups
}

func selectTargets(groups: [Group]) -> [Int: Int] {
    var targets: [Int: Int] = [:]
    var targeted: Set<Int> = []
    
    let sortedGroups = groups.filter { $0.units > 0 }.sorted {
        if $0.effectivePower == $1.effectivePower {
            return $0.initiative > $1.initiative
        }
        return $0.effectivePower > $1.effectivePower
    }

    for attacker in sortedGroups {
        let enemies = groups.filter { $0.army != attacker.army && $0.units > 0 }
        var bestTarget: Group?
        var maxDamage = 0

        for defender in enemies {
            if targeted.contains(defender.id) { continue }
            let damage = attacker.damageDealt(to: defender)
            if damage > maxDamage {
                maxDamage = damage
                bestTarget = defender
            } else if damage == maxDamage, let currentBest = bestTarget {
                if defender.effectivePower > currentBest.effectivePower {
                    bestTarget = defender
                } else if defender.effectivePower == currentBest.effectivePower && defender.initiative > currentBest.initiative {
                    bestTarget = defender
                }
            }
        }

        if let target = bestTarget, maxDamage > 0 {
            targets[attacker.id] = target.id
            targeted.insert(target.id)
        }
    }

    return targets
}

func performAttacks(groups: inout [Group], targets: [Int: Int]) -> Bool{
    let sortedAttackers = groups.filter{$0.units > 0}.sorted { $0.initiative > $1.initiative }
    var unitsKilled = false
    for attacker in sortedAttackers {
        guard let targetId = targets[attacker.id], let targetIndex = groups.firstIndex(where: { $0.id == targetId }), groups[targetIndex].units > 0 else { continue }
        
        
        if let attackerIndex = groups.firstIndex(where: { $0.id == attacker.id }) {
            if groups[attackerIndex].units <= 0 {
                continue
            }
            let damage = groups[attackerIndex].damageDealt(to: groups[targetIndex])
            let unitsLost = min(groups[targetIndex].units, damage / groups[targetIndex].hitPoints)
            groups[targetIndex].units -= unitsLost
            if unitsLost > 0 { unitsKilled = true }
        }

    }
    return unitsKilled
}

func simulateCombat(groups: [Group]) -> (String, Int) {
    var currentGroups = groups

    while Set(currentGroups.map { $0.army }).count > 1 {
       
        let targets = selectTargets(groups: currentGroups)
        if !performAttacks(groups: &currentGroups, targets: targets) {
            // Stalemate, return a default result to avoid infinite loop.
            return ("Stalemate", 0)
        }
        currentGroups = currentGroups.filter { $0.units > 0 }
    }

    let winningArmy = currentGroups.first!.army
    let totalUnits = currentGroups.reduce(0) { $0 + $1.units }
    return (winningArmy, totalUnits)
}

func main() {
    do {
        let input = try String(contentsOfFile: "input.txt")
        let groups = parseInput(input)
        let (winningArmy, unitsRemaining) = simulateCombat(groups: groups)
        print("\(unitsRemaining)")

    } catch {
        print("Error reading input file: \(error)")
    }
}

main()
