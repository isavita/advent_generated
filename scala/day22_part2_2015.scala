
import scala.io.Source

case class GameState(playerHP: Int, playerMana: Int, bossHP: Int, bossDamage: Int, 
                     shieldTimer: Int, poisonTimer: Int, rechargeTimer: Int, 
                     manaSpent: Int)

object Game {
  def minManaToWin(initialState: GameState): Int = {
    var minMana = Int.MaxValue

    def simulate(state: GameState, playerTurn: Boolean): Unit = {
      if (state.manaSpent >= minMana || state.playerHP <= 0) return
      if (state.bossHP <= 0) {
        minMana = state.manaSpent
        return
      }

      val newState = if (playerTurn) state.copy(playerHP = state.playerHP - 1) else state

      val updatedState = applyEffects(newState)

      if (!playerTurn) {
        val damage = Math.max(1, updatedState.bossDamage - (if (updatedState.shieldTimer > 0) 7 else 0))
        simulate(updatedState.copy(playerHP = updatedState.playerHP - damage), true)
      } else {
        castSpells(updatedState)
      }
    }

    def applyEffects(state: GameState): GameState = {
      state.copy(
        bossHP = state.bossHP - (if (state.poisonTimer > 0) 3 else 0),
        playerMana = state.playerMana + (if (state.rechargeTimer > 0) 101 else 0),
        shieldTimer = Math.max(0, state.shieldTimer - 1),
        poisonTimer = Math.max(0, state.poisonTimer - 1),
        rechargeTimer = Math.max(0, state.rechargeTimer - 1)
      )
    }

    def castSpells(state: GameState): Unit = {
      if (state.playerMana >= 53) simulate(state.copy(playerMana = state.playerMana - 53, bossHP = state.bossHP - 4, manaSpent = state.manaSpent + 53), false)
      if (state.playerMana >= 73) simulate(state.copy(playerMana = state.playerMana - 73, playerHP = state.playerHP + 2, bossHP = state.bossHP - 2, manaSpent = state.manaSpent + 73), false)
      if (state.playerMana >= 113 && state.shieldTimer == 0) simulate(state.copy(playerMana = state.playerMana - 113, shieldTimer = 6, manaSpent = state.manaSpent + 113), false)
      if (state.playerMana >= 173 && state.poisonTimer == 0) simulate(state.copy(playerMana = state.playerMana - 173, poisonTimer = 6, manaSpent = state.manaSpent + 173), false)
      if (state.playerMana >= 229 && state.rechargeTimer == 0) simulate(state.copy(playerMana = state.playerMana - 229, rechargeTimer = 5, manaSpent = state.manaSpent + 229), false)
    }

    simulate(initialState.copy(playerHP = 50, playerMana = 500), true)
    minMana
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val bossHP = lines(0).split(": ")(1).toInt
    val bossDamage = lines(1).split(": ")(1).toInt

    val initialState = GameState(50, 500, bossHP, bossDamage, 0, 0, 0, 0)
    println(minManaToWin(initialState))
  }
}
