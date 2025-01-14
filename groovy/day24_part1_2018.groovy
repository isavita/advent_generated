
import java.util.regex.Matcher
import java.util.regex.Pattern

class Group {
    int id
    String army
    int units
    int hitPoints
    Set<String> weaknesses = [] as Set
    Set<String> immunities = [] as Set
    int attackDamage
    String attackType
    int initiative

    int getEffectivePower() {
        units * attackDamage
    }

    int calculateDamage(Group defender) {
        int damage = effectivePower
        if (defender.immunities.contains(attackType)) {
            return 0
        }
        if (defender.weaknesses.contains(attackType)) {
            return damage * 2
        }
        return damage
    }

    void takeDamage(int damage) {
        int unitsLost = damage / hitPoints
        units = Math.max(0, units - unitsLost)
    }

    @Override
    String toString() {
        return "Group{id=$id, army='$army', units=$units, hitPoints=$hitPoints, weaknesses=$weaknesses, immunities=$immunities, attackDamage=$attackDamage, attackType='$attackType', initiative=$initiative}"
    }
}

def parseGroup(String line, String army, int id) {
    Pattern pattern = Pattern.compile("(\\d+) units each with (\\d+) hit points(?: \\(([^)]+)\\))? with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)")
    Matcher matcher = pattern.matcher(line)
    if (matcher.find()) {
        def group = new Group(
                id: id,
                army: army,
                units: matcher.group(1).toInteger(),
                hitPoints: matcher.group(2).toInteger(),
                attackDamage: matcher.group(4).toInteger(),
                attackType: matcher.group(5),
                initiative: matcher.group(6).toInteger()
        )
        if (matcher.group(3)) {
            def parts = matcher.group(3).split('; ')
            parts.each { part ->
                if (part.startsWith('weak to ')) {
                    group.weaknesses.addAll(part.substring('weak to '.length()).split(', '))
                } else if (part.startsWith('immune to ')) {
                    group.immunities.addAll(part.substring('immune to '.length()).split(', '))
                }
            }
        }
        return group
    }
    return null
}

def parseInput(File inputFile) {
    def immuneSystem = []
    def infection = []
    def currentArmy = null
    int id = 1
    inputFile.eachLine { line ->
        if (line.startsWith('Immune System:')) {
            currentArmy = immuneSystem
        } else if (line.startsWith('Infection:')) {
            currentArmy = infection
        } else if (line.trim()) {
            def group = parseGroup(line, currentArmy == immuneSystem ? 'immune' : 'infection', id++)
            if (group) {
                currentArmy << group
            }
        }
    }
    return [immuneSystem, infection]
}

def selectTargets(List<Group> attackers, List<Group> defenders) {
    def targets = [:]
    def targeted = [] as Set
    attackers.sort { a, b ->
        def powerDiff = b.effectivePower <=> a.effectivePower
        if (powerDiff != 0) return powerDiff
        return b.initiative <=> a.initiative
    }.each { attacker ->
        def bestTarget = null
        int maxDamage = 0
        def maxPower = 0
        int maxInitiative = 0
        def possibleTargets = defenders.findAll { !targeted.contains(it) && it.units > 0 }
        possibleTargets.each { defender ->
            int damage = attacker.calculateDamage(defender)
            if (damage > maxDamage) {
                maxDamage = damage
                bestTarget = defender
                maxPower = defender.effectivePower
                maxInitiative = defender.initiative
            } else if (damage == maxDamage) {
                if (defender.effectivePower > maxPower) {
                    bestTarget = defender
                    maxPower = defender.effectivePower
                    maxInitiative = defender.initiative
                } else if (defender.effectivePower == maxPower && defender.initiative > maxInitiative) {
                    bestTarget = defender
                    maxInitiative = defender.initiative
                }
            }
        }
        if (bestTarget) {
            targets[attacker] = bestTarget
            targeted << bestTarget
        }
    }
    return targets
}

def attack(Map<Group, Group> targets) {
    targets.keySet().sort { a, b -> b.initiative <=> a.initiative }.each { attacker ->
        if (attacker.units <= 0) return
        def defender = targets[attacker]
        if (defender && defender.units > 0) {
            int damage = attacker.calculateDamage(defender)
            defender.takeDamage(damage)
        }
    }
}

def simulateCombat(List<Group> immuneSystem, List<Group> infection) {
    while (immuneSystem.any { it.units > 0 } && infection.any { it.units > 0 }) {
        def targets = selectTargets(immuneSystem.findAll { it.units > 0 }, infection.findAll { it.units > 0 })
        targets.putAll(selectTargets(infection.findAll { it.units > 0 }, immuneSystem.findAll { it.units > 0 }))
        if (targets.isEmpty()) {
            return 0
        }
        attack(targets)
        if (immuneSystem.every { it.units == 0 } || infection.every { it.units == 0 }) {
            break
        }
    }
    return immuneSystem.sum { it.units } + infection.sum { it.units }
}

def inputFile = new File('input.txt')
def (immuneSystem, infection) = parseInput(inputFile)
def result = simulateCombat(immuneSystem, infection)
println result
