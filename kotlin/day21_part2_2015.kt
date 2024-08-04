import java.io.File

data class Item(val name: String, val cost: Int, val damage: Int, val armor: Int)
data class Character(val hitPoints: Int, val damage: Int, val armor: Int)

fun simulateBattle(player: Character, boss: Character): Boolean {
    var playerHP = player.hitPoints
    var bossHP = boss.hitPoints

    while (true) {
        bossHP -= maxOf(1, player.damage - boss.armor)
        if (bossHP <= 0) return true

        playerHP -= maxOf(1, boss.damage - player.armor)
        if (playerHP <= 0) return false
    }
}

fun main() {
    val input = File("input.txt").readLines()
    val bossHitPoints = input[0].split(": ")[1].toInt()
    val bossDamage = input[1].split(": ")[1].toInt()
    val bossArmor = input[2].split(": ")[1].toInt()

    val boss = Character(bossHitPoints, bossDamage, bossArmor)

    val weapons = listOf(
        Item("Dagger", 8, 4, 0),
        Item("Shortsword", 10, 5, 0),
        Item("Warhammer", 25, 6, 0),
        Item("Longsword", 40, 7, 0),
        Item("Greataxe", 74, 8, 0)
    )

    val armors = listOf(
        Item("None", 0, 0, 0),
        Item("Leather", 13, 0, 1),
        Item("Chainmail", 31, 0, 2),
        Item("Splintmail", 53, 0, 3),
        Item("Bandedmail", 75, 0, 4),
        Item("Platemail", 102, 0, 5)
    )

    val rings = listOf(
        Item("None1", 0, 0, 0),
        Item("None2", 0, 0, 0),
        Item("Damage +1", 25, 1, 0),
        Item("Damage +2", 50, 2, 0),
        Item("Damage +3", 100, 3, 0),
        Item("Defense +1", 20, 0, 1),
        Item("Defense +2", 40, 0, 2),
        Item("Defense +3", 80, 0, 3)
    )

    var minGoldToWin = Int.MAX_VALUE
    var maxGoldToLose = 0

    for (weapon in weapons) {
        for (armor in armors) {
            for (ring1 in rings) {
                for (ring2 in rings) {
                    if (ring1 == ring2 && ring1.name != "None1") continue

                    val totalCost = weapon.cost + armor.cost + ring1.cost + ring2.cost
                    val totalDamage = weapon.damage + armor.damage + ring1.damage + ring2.damage
                    val totalArmor = weapon.armor + armor.armor + ring1.armor + ring2.armor

                    val player = Character(100, totalDamage, totalArmor)

                    if (simulateBattle(player, boss)) {
                        minGoldToWin = minOf(minGoldToWin, totalCost)
                    } else {
                        maxGoldToLose = maxOf(maxGoldToLose, totalCost)
                    }
                }
            }
        }
    }

    println("Least amount of gold to win: $minGoldToWin")
    println("Most amount of gold to lose: $maxGoldToLose")
}