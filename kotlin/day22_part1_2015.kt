import java.io.File

data class GameState(
    var playerHP: Int = 0,
    var playerMana: Int = 0,
    var bossHP: Int = 0,
    var bossDamage: Int = 0,
    var shieldTimer: Int = 0,
    var poisonTimer: Int = 0,
    var rechargeTimer: Int = 0,
    var manaSpent: Int = 0
)

fun minManaToWin(initialState: GameState): Int {
    var minMana = Int.MAX_VALUE
    fun simulate(state: GameState, playerTurn: Boolean) {
        if (state.manaSpent >= minMana) return
        if (state.bossHP <= 0) {
            minMana = state.manaSpent
            return
        }
        if (state.playerHP <= 0) return

        if (state.shieldTimer > 0) state.shieldTimer--
        if (state.poisonTimer > 0) {
            state.bossHP -= 3
            state.poisonTimer--
        }
        if (state.rechargeTimer > 0) {
            state.playerMana += 101
            state.rechargeTimer--
        }

        if (!playerTurn) {
            var damage = state.bossDamage
            if (state.shieldTimer > 0) damage -= 7
            if (damage < 1) damage = 1
            state.playerHP -= damage
            simulate(state, true)
            return
        }

        if (state.playerMana >= 53) {
            val newState = state.copy(playerMana = state.playerMana - 53, manaSpent = state.manaSpent + 53, bossHP = state.bossHP - 4)
            simulate(newState, false)
        }
        if (state.playerMana >= 73) {
            val newState = state.copy(playerMana = state.playerMana - 73, manaSpent = state.manaSpent + 73, bossHP = state.bossHP - 2, playerHP = state.playerHP + 2)
            simulate(newState, false)
        }
        if (state.playerMana >= 113 && state.shieldTimer == 0) {
            val newState = state.copy(playerMana = state.playerMana - 113, manaSpent = state.manaSpent + 113, shieldTimer = 6)
            simulate(newState, false)
        }
        if (state.playerMana >= 173 && state.poisonTimer == 0) {
            val newState = state.copy(playerMana = state.playerMana - 173, manaSpent = state.manaSpent + 173, poisonTimer = 6)
            simulate(newState, false)
        }
        if (state.playerMana >= 229 && state.rechargeTimer == 0) {
            val newState = state.copy(playerMana = state.playerMana - 229, manaSpent = state.manaSpent + 229, rechargeTimer = 5)
            simulate(newState, false)
        }
    }

    initialState.playerHP = 50
    initialState.playerMana = 500
    simulate(initialState, true)
    return minMana
}

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    val bossHP = lines[0].split(": ")[1].toInt()
    val bossDamage = lines[1].split(": ")[1].toInt()

    val initialState = GameState(bossHP = bossHP, bossDamage = bossDamage)
    println(minManaToWin(initialState))
}