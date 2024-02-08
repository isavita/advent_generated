
object Day21 extends App {
  case class Item(name: String, cost: Int, damage: Int, armor: Int)
  
  val weapons = List(
    Item("Dagger", 8, 4, 0),
    Item("Shortsword", 10, 5, 0),
    Item("Warhammer", 25, 6, 0),
    Item("Longsword", 40, 7, 0),
    Item("Greataxe", 74, 8, 0)
  )
  
  val armor = List(
    Item("Leather", 13, 0, 1),
    Item("Chainmail", 31, 0, 2),
    Item("Splintmail", 53, 0, 3),
    Item("Bandedmail", 75, 0, 4),
    Item("Platemail", 102, 0, 5)
  )
  
  val rings = List(
    Item("Damage +1", 25, 1, 0),
    Item("Damage +2", 50, 2, 0),
    Item("Damage +3", 100, 3, 0),
    Item("Defense +1", 20, 0, 1),
    Item("Defense +2", 40, 0, 2),
    Item("Defense +3", 80, 0, 3)
  )
  
  val boss = List(103, 9, 2) // Hit points, damage, armor
  
  def simulateFight(playerDamage: Int, playerArmor: Int): Boolean = {
    var playerHitPoints = 100
    var bossHitPoints = boss(0)
    
    var playerTurn = true
    
    while (playerHitPoints > 0 && bossHitPoints > 0) {
      if (playerTurn) {
        val damageDealt = Math.max(1, playerDamage - boss(2))
        bossHitPoints -= damageDealt
      } else {
        val damageDealt = Math.max(1, boss(1) - playerArmor)
        playerHitPoints -= damageDealt
      }
      
      playerTurn = !playerTurn
    }
    
    playerHitPoints > 0
  }
  
  val allItems = weapons ++ armor ++ rings
  
  val itemCombinations = for {
    weapon <- weapons
    armor <- armor :+ Item("None", 0, 0, 0)
    ring1 <- rings :+ Item("None", 0, 0, 0)
    ring2 <- rings.filterNot(_ == ring1) :+ Item("None", 0, 0, 0)
  } yield List(weapon, armor, ring1, ring2)
  
  val validItemCombinations = itemCombinations.filter { items =>
    val totalCost = items.map(_.cost).sum
    val totalDamage = items.map(_.damage).sum
    val totalArmor = items.map(_.armor).sum
    
    simulateFight(totalDamage, totalArmor)
  }
  
  val minCost = validItemCombinations.map(_.map(_.cost).sum).min
  println(minCost)
}
