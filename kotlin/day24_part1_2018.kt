
import java.io.File

data class Group(
    val id: Int,
    val army: String,
    var units: Int,
    val hitPoints: Int,
    val attackDamage: Int,
    val attackType: String,
    val initiative: Int,
    val weaknesses: Set<String>,
    val immunities: Set<String>
) {
    val effectivePower: Int
        get() = units * attackDamage
}

fun main() {
    val input = File("input.txt").readLines()
    val (immuneSystem, infection) = parseInput(input)

    val result = simulateCombat(immuneSystem, infection)
    println(result)
}

fun parseInput(input: List<String>): Pair<List<Group>, List<Group>> {
    val immuneSystem = mutableListOf<Group>()
    val infection = mutableListOf<Group>()
    var currentArmy = ""
    var id = 1

    for (line in input) {
        when {
            line.startsWith("Immune System:") -> currentArmy = "Immune System"
            line.startsWith("Infection:") -> currentArmy = "Infection"
            line.isBlank() -> continue
            else -> {
                val group = parseGroup(line, id++, currentArmy)
                if (currentArmy == "Immune System") {
                    immuneSystem.add(group)
                } else {
                    infection.add(group)
                }
            }
        }
    }

    return immuneSystem to infection
}

fun parseGroup(line: String, id: Int, army: String): Group {
    val units = line.substringBefore(" units").toInt()
    val hitPoints = line.substringAfter("each with ").substringBefore(" hit points").toInt()
    val attackDamage = line.substringAfter("with an attack that does ").substringBefore(" ").toInt()
    val attackType = line.substringAfter("$attackDamage ").substringBefore(" damage")
    val initiative = line.substringAfter("initiative ").toInt()

    val weaknesses = mutableSetOf<String>()
    val immunities = mutableSetOf<String>()
    if ("(" in line) {
        val modifiers = line.substringAfter("(").substringBefore(")").split("; ")
        for (modifier in modifiers) {
            if (modifier.startsWith("weak to ")) {
                weaknesses.addAll(modifier.substringAfter("weak to ").split(", "))
            } else if (modifier.startsWith("immune to ")) {
                immunities.addAll(modifier.substringAfter("immune to ").split(", "))
            }
        }
    }

    return Group(id, army, units, hitPoints, attackDamage, attackType, initiative, weaknesses, immunities)
}

fun simulateCombat(immuneSystem: List<Group>, infection: List<Group>): Int {
    val allGroups = (immuneSystem + infection).toMutableList()

    while (allGroups.any { it.army == "Immune System" && it.units > 0 } &&
        allGroups.any { it.army == "Infection" && it.units > 0 }) {

        // Target selection
        val targets = mutableMapOf<Int, Int?>()
        val targetedBy = mutableSetOf<Int>()
        val sortedGroups = allGroups.filter { it.units > 0 }.sortedWith(
            compareByDescending<Group> { it.effectivePower }.thenByDescending { it.initiative }
        )

        for (attacker in sortedGroups) {
            val potentialTargets = allGroups.filter {
                it.units > 0 && it.army != attacker.army && it.id !in targetedBy
            }
            val bestTarget = potentialTargets.maxWithOrNull(
                compareBy<Group> { calculateDamage(attacker, it) }
                    .thenBy { it.effectivePower }
                    .thenBy { it.initiative }
            )

            if (bestTarget != null && calculateDamage(attacker, bestTarget) > 0) {
                targets[attacker.id] = bestTarget.id
                targetedBy.add(bestTarget.id)
            } else {
                targets[attacker.id] = null
            }
        }

        // Attacking
        val attackingOrder = allGroups.filter { it.units > 0 }.sortedByDescending { it.initiative }
        for (attacker in attackingOrder) {
            val targetId = targets[attacker.id] ?: continue
            val target = allGroups.find { it.id == targetId } ?: continue
            if (attacker.units <= 0) continue

            val damage = calculateDamage(attacker, target)
            val unitsKilled = minOf(target.units, damage / target.hitPoints)
            target.units -= unitsKilled
        }
        allGroups.removeIf { it.units <= 0 }
    }

    return allGroups.sumOf { it.units }
}

fun calculateDamage(attacker: Group, defender: Group): Int {
    if (defender.immunities.contains(attacker.attackType)) {
        return 0
    }
    return if (defender.weaknesses.contains(attacker.attackType)) {
        attacker.effectivePower * 2
    } else {
        attacker.effectivePower
    }
}
