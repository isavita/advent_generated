
import java.io.File
import java.util.regex.Pattern

data class Group(
    var units: Int,
    val hitPoints: Int,
    var attackDamage: Int,
    val attackType: String,
    val initiative: Int,
    val immunities: List<String> = emptyList(),
    val weaknesses: List<String> = emptyList()
) {
    var attacker: Group? = null
    var target: Group? = null

    fun effectivePower(): Int = units * attackDamage

    fun damageDealt(e: Group): Int {
        if (e.immunities.contains(attackType)) return 0
        return if (e.weaknesses.contains(attackType)) effectivePower() * 2 else effectivePower()
    }
}

class Initiative(private val groups: MutableList<Group>) : MutableList<Group> by groups {
    fun attack() {
        groups.sortedByDescending { it.initiative }.forEach { group ->
            if (group.units > 0 && group.target != null && group.target!!.units > 0) {
                group.target!!.units -= group.damageDealt(group.target!!) / group.target!!.hitPoints
            }
            group.target?.attacker = null
            group.target = null
        }
    }

    fun clean() {
        val c = groups.filter { it.units > 0 }.toMutableList()
        c.sortByDescending { it.initiative }
        groups.clear()
        groups.addAll(c)
    }
}

class Army(private val groups: MutableList<Group>) : MutableList<Group> by groups {
    fun alive(): Boolean = groups.any { it.units > 0 }
    fun boost(amount: Int) = groups.forEach { it.attackDamage += amount }
}

class Battlefield(private val armies: MutableMap<Int, Army>) : MutableMap<Int, Army> by armies {
    fun findTargets() {
        armies.forEach { (army, groups) ->
            groups.sortedWith(compareByDescending<Group> { it.effectivePower() }.thenByDescending { it.initiative })
                .forEach { group ->
                    armies.filter { it.key != army && group.units > 0 }.values.forEach { enemyGroups ->
                        var mostDamage = 0
                        var targetGroup: Group? = null
                        enemyGroups.filter { it.units > 0 && it.attacker == null && group.damageDealt(it) > 0 }
                            .forEach { enemyGroup ->
                                val damage = group.damageDealt(enemyGroup)
                                if (damage > mostDamage || (damage == mostDamage && targetGroup != null &&
                                            (enemyGroup.effectivePower() > targetGroup!!.effectivePower() ||
                                                    (enemyGroup.effectivePower() == targetGroup!!.effectivePower() &&
                                                            enemyGroup.initiative > targetGroup!!.initiative)))
                                ) {
                                    mostDamage = damage
                                    targetGroup = enemyGroup
                                }
                            }
                        targetGroup?.let {
                            group.target = it
                            it.attacker = group
                        }
                    }
                }
        }
    }

    fun clean() = armies.forEach { (_, army) -> army.retainAll { it.units > 0 } }
    fun active(): Boolean = armies.values.all { it.alive() }
    fun result(): Pair<Int, Int> = armies.entries.first { it.value.alive() }
        .let { (army, groups) -> army to groups.sumOf { it.units } }

    fun totalUnits(): Int = armies.values.sumOf { army -> army.sumOf { it.units } }
}

object Constants {
    const val ARMY_IMMUNE_SYSTEM = 1
    const val ARMY_INFECTION = 2
}

fun prepareForBattle(input: List<String>): Pair<Battlefield, Initiative> {
    val initiative = mutableListOf<Group>()
    val battle = mutableMapOf<Int, Army>()
    var currentArmy = 0
    val armyName = Pattern.compile("^(.*):$")
    val groupImmunities = Pattern.compile("immune to (.*?)[;)]")
    val groupWeaknesses = Pattern.compile("weak to (.*?)[;)]")
    val groupDescription =
        Pattern.compile("^(\\d+) units each with (\\d+) hit points.*with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)$")
    val stringArmies = mapOf("Immune System" to Constants.ARMY_IMMUNE_SYSTEM, "Infection" to Constants.ARMY_INFECTION)

    input.forEach { line ->
        armyName.matcher(line).takeIf { it.matches() }?.let {
            currentArmy = stringArmies[it.group(1)] ?: error("Unknown army: ${it.group(1)}")
        } ?: run {
            if (currentArmy !in Constants.ARMY_IMMUNE_SYSTEM..Constants.ARMY_INFECTION) {
                error("Tried to assign group to invalid army: $currentArmy")
            }
            val description = groupDescription.matcher(line)
            if (!description.matches()) return@forEach
            val group = Group(
                units = description.group(1).toInt(),
                hitPoints = description.group(2).toInt(),
                attackDamage = description.group(3).toInt(),
                attackType = description.group(4),
                initiative = description.group(5).toInt(),
                immunities = groupImmunities.matcher(line).takeIf { it.find() }?.group(1)?.split(", ") ?: emptyList(),
                weaknesses = groupWeaknesses.matcher(line).takeIf { it.find() }?.group(1)?.split(", ") ?: emptyList()
            )
            battle.getOrPut(currentArmy) { Army(mutableListOf()) }.add(group)
            initiative.add(group)
        }
    }
    return Battlefield(battle) to Initiative(initiative)
}

fun immuneSystemBoost(input: List<String>): Int {
    var boost = 0
    while (true) {
        var stalemate = false
        val (battle, initiative) = prepareForBattle(input)
        battle[Constants.ARMY_IMMUNE_SYSTEM]?.boost(boost)
        while (battle.active()) {
            val before = battle.totalUnits()
            battle.findTargets()
            initiative.attack()
            if (battle.totalUnits() == before) {
                stalemate = true
                break
            }
            battle.clean()
            initiative.clean()
        }
        if (!stalemate) {
            val (winner, units) = battle.result()
            if (winner == Constants.ARMY_IMMUNE_SYSTEM) return units
        }
        boost++
    }
}

fun main() {
    val input = File("input.txt").readLines()
    println(immuneSystemBoost(input))
}
