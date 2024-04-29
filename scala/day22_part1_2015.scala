import scala.io.Source

case class GameState(playerHP: Int, playerMana: Int, bossHP: Int, bossDamage: Int, shieldTimer: Int, poisonTimer: Int, rechargeTimer: Int, manaSpent: Int)

object Main {
  var minMana = Int.MaxValue

  def simulate(state: GameState, playerTurn: Boolean): Unit = {
    if (state.manaSpent >= minMana) return
    if (state.bossHP <= 0) {
      minMana = state.manaSpent
      return
    }
    if (state.playerHP <= 0) return

    // Apply effects
    var newState = state
    if (newState.shieldTimer > 0) newState = newState.copy(shieldTimer = newState.shieldTimer - 1)
    if (newState.poisonTimer > 0) {
      newState = newState.copy(bossHP = newState.bossHP - 3)
      newState = newState.copy(poisonTimer = newState.poisonTimer - 1)
    }
    if (newState.rechargeTimer > 0) {
      newState = newState.copy(playerMana = newState.playerMana + 101)
      newState = newState.copy(rechargeTimer = newState.rechargeTimer - 1)
    }

    if (!playerTurn) {
      var damage = newState.bossDamage
      if (newState.shieldTimer > 0) damage -= 7
      if (damage < 1) damage = 1
      newState = newState.copy(playerHP = newState.playerHP - damage)
      simulate(newState, true)
      return
    }

    if (newState.playerMana >= 53) {
      var newNewState = newState.copy(playerMana = newState.playerMana - 53, manaSpent = newState.manaSpent + 53)
      newNewState = newNewState.copy(bossHP = newNewState.bossHP - 4)
      simulate(newNewState, false)
    }
    if (newState.playerMana >= 73) {
      var newNewState = newState.copy(playerMana = newState.playerMana - 73, manaSpent = newState.manaSpent + 73)
      newNewState = newNewState.copy(bossHP = newNewState.bossHP - 2, playerHP = newNewState.playerHP + 2)
      simulate(newNewState, false)
    }
    if (newState.playerMana >= 113 && newState.shieldTimer == 0) {
      var newNewState = newState.copy(playerMana = newState.playerMana - 113, manaSpent = newState.manaSpent + 113)
      newNewState = newNewState.copy(shieldTimer = 6)
      simulate(newNewState, false)
    }
    if (newState.playerMana >= 173 && newState.poisonTimer == 0) {
      var newNewState = newState.copy(playerMana = newState.playerMana - 173, manaSpent = newState.manaSpent + 173)
      newNewState = newNewState.copy(poisonTimer = 6)
      simulate(newNewState, false)
    }
    if (newState.playerMana >= 229 && newState.rechargeTimer == 0) {
      var newNewState = newState.copy(playerMana = newState.playerMana - 229, manaSpent = newState.manaSpent + 229)
      newNewState = newNewState.copy(rechargeTimer = 5)
      simulate(newNewState, false)
    }
  }

  def main(args: Array[String]): Unit = {
    val file = Source.fromFile("input.txt")
    val lines = file.getLines().toArray
    val bossHP = lines(0).split(": ")(1).toInt
    val bossDamage = lines(1).split(": ")(1).toInt

    val initialState = GameState(50, 500, bossHP, bossDamage, 0, 0, 0, 0)
    simulate(initialState, true)
    println(minMana)
  }
}