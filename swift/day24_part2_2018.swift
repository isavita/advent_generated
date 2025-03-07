
import Foundation

class Group {
    var units: Int
    let hitPoints: Int
    var attackDamage: Int
    let attackType: String
    let initiative: Int
    let immunities: Set<String>
    let weaknesses: Set<String>
    weak var attacker: Group?
    weak var target: Group?

    init(units: Int, hitPoints: Int, attackDamage: Int, attackType: String, initiative: Int, immunities: Set<String>, weaknesses: Set<String>) {
        self.units = units
        self.hitPoints = hitPoints
        self.attackDamage = attackDamage
        self.attackType = attackType
        self.initiative = initiative
        self.immunities = immunities
        self.weaknesses = weaknesses
    }

    func effectivePower() -> Int {
        return units * attackDamage
    }

    func damageDealt(to enemy: Group) -> Int {
        if enemy.immunities.contains(attackType) {
            return 0
        }
        if enemy.weaknesses.contains(attackType) {
            return effectivePower() * 2
        }
        return effectivePower()
    }
}

class Army {
    var groups: [Group]

    init(groups: [Group]) {
        self.groups = groups
    }

    func isAlive() -> Bool {
        return groups.contains(where: { $0.units > 0 })
    }

    func boost(amount: Int) {
        for group in groups {
            group.attackDamage += amount
        }
    }
}

class Battlefield {
    var armies: [Int: Army]

    init(armies: [Int: Army]) {
        self.armies = armies
    }

    func findTargets() {
        for (armyId, army) in armies {
            army.groups.sort { (g1, g2) -> Bool in
                if g1.effectivePower() != g2.effectivePower() {
                    return g1.effectivePower() > g2.effectivePower()
                }
                return g1.initiative > g2.initiative
            }

            for group in army.groups {
                guard group.units > 0 else { continue }
                var mostDamage = 0
                var targetGroup: Group?

                for (enemyArmyId, enemyArmy) in armies {
                    guard armyId != enemyArmyId else { continue }

                    for enemyGroup in enemyArmy.groups {
                        guard enemyGroup.units > 0 && enemyGroup.attacker == nil else { continue }
                        let damage = group.damageDealt(to: enemyGroup)
                        guard damage > 0 else { continue }

                        if damage > mostDamage {
                            mostDamage = damage
                            targetGroup = enemyGroup
                        } else if damage == mostDamage, let currentTarget = targetGroup {
                            if enemyGroup.effectivePower() > currentTarget.effectivePower() {
                                targetGroup = enemyGroup
                            } else if enemyGroup.effectivePower() == currentTarget.effectivePower() && enemyGroup.initiative > currentTarget.initiative {
                                targetGroup = enemyGroup
                            }
                        }
                    }
                }

                if let target = targetGroup {
                    group.target = target
                    target.attacker = group
                }
            }
        }
    }

    func attack() {
        var allGroups: [Group] = []
        for army in armies.values {
            allGroups.append(contentsOf: army.groups)
        }
        allGroups.sort { $0.initiative > $1.initiative }

        for group in allGroups {
            if group.units > 0, let target = group.target, target.units > 0 {
                let damage = group.damageDealt(to: target)
                let unitsKilled = min(target.units, damage / target.hitPoints)
                target.units -= unitsKilled
                group.target = nil
                target.attacker = nil
            }
        }
    }

    func clean() {
        for armyId in armies.keys {
            armies[armyId]?.groups.removeAll(where: { $0.units <= 0 })
        }
    }

    func isActive() -> Bool {
        return armies.values.allSatisfy { $0.isAlive() }
    }

    func result() -> (winner: Int, units: Int) {
        for (armyId, army) in armies {
            if army.isAlive() {
                let units = army.groups.reduce(0) { $0 + $1.units }
                return (winner: armyId, units: units)
            }
        }
        return (winner: 0, units: 0)
    }

    func totalUnits() -> Int {
        return armies.values.flatMap { $0.groups }.reduce(0) { $0 + $1.units }
    }
}

func parseInput(input: String) -> Battlefield {
    var armies: [Int: Army] = [:]
    var currentArmyId = 0

    let lines = input.components(separatedBy: .newlines)

    for line in lines {
        if line.contains("Immune System") {
            currentArmyId = 1
        } else if line.contains("Infection") {
            currentArmyId = 2
        } else if line.isEmpty {
            continue
        } else {
            let components = line.components(separatedBy: .whitespaces)
            guard components.count > 10 else { continue }

            let units = Int(components[0])!
            let hitPoints = Int(components[4])!
            let attackDamage = Int(components[components.count - 6])!
            let attackType = components[components.count - 5]
            let initiative = Int(components[components.count - 1])!

            var immunities: Set<String> = []
            var weaknesses: Set<String> = []

            if line.contains("(") {
                let properties = line.components(separatedBy: "(")[1].components(separatedBy: ")")[0]
                let parts = properties.components(separatedBy: "; ")

                for part in parts {
                    if part.contains("immune to") {
                        let items = part.components(separatedBy: "immune to ")[1].components(separatedBy: ", ")
                        immunities = Set(items)
                    } else if part.contains("weak to") {
                        let items = part.components(separatedBy: "weak to ")[1].components(separatedBy: ", ")
                        weaknesses = Set(items)
                    }
                }
            }

            let group = Group(units: units, hitPoints: hitPoints, attackDamage: attackDamage, attackType: attackType, initiative: initiative, immunities: immunities, weaknesses: weaknesses)

            if armies[currentArmyId] == nil {
                armies[currentArmyId] = Army(groups: [])
            }
            armies[currentArmyId]?.groups.append(group)
        }
    }

    return Battlefield(armies: armies)
}

func immuneSystemBoost(input: String) -> Int {
    var boost = 0
    while true {
        var stalemate = false
        let battle = parseInput(input: input)

        battle.armies[1]?.boost(amount: boost)

        while battle.isActive() {
            let before = battle.totalUnits()

            battle.findTargets()
            battle.attack()

            if battle.totalUnits() == before {
                stalemate = true
                break
            }

            battle.clean()
        }

        if !stalemate {
            let result = battle.result()
            if result.winner == 1 {
                return result.units
            }
        }

        boost += 1
    }
}

func main() {
    do {
        let input = try String(contentsOfFile: "input.txt", encoding: .utf8).trimmingCharacters(in: .whitespacesAndNewlines)
        let result = immuneSystemBoost(input: input)
        print(result)
    } catch {
        print("Error reading input file: \(error)")
    }
}

main()
