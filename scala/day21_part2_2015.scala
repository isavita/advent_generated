
object Main extends App {
  val weapons = List(
    ("Dagger", 8, 4, 0),
    ("Shortsword", 10, 5, 0),
    ("Warhammer", 25, 6, 0),
    ("Longsword", 40, 7, 0),
    ("Greataxe", 74, 8, 0)
  )

  val armor = List(
    ("Leather", 13, 0, 1),
    ("Chainmail", 31, 0, 2),
    ("Splintmail", 53, 0, 3),
    ("Bandedmail", 75, 0, 4),
    ("Platemail", 102, 0, 5)
  )

  val rings = List(
    ("Damage +1", 25, 1, 0),
    ("Damage +2", 50, 2, 0),
    ("Damage +3", 100, 3, 0),
    ("Defense +1", 20, 0, 1),
    ("Defense +2", 40, 0, 2),
    ("Defense +3", 80, 0, 3)
  )

  val bossHitPoints = 103
  val bossDamage = 9
  val bossArmor = 2

  val playerHitPoints = 100

  val allItems = weapons ++ armor ++ rings

  def fight(playerDamage: Int, playerArmor: Int): Boolean = {
    var playerHP = playerHitPoints
    var bossHP = bossHitPoints

    var playerTurn = true

    while (playerHP > 0 && bossHP > 0) {
      if (playerTurn) {
        val damage = (playerDamage - bossArmor).max(1)
        bossHP -= damage
      } else {
        val damage = (bossDamage - playerArmor).max(1)
        playerHP -= damage
      }

      playerTurn = !playerTurn
    }

    if (playerHP > 0) true
    else false
  }

  val winningCosts = for {
    weapon <- weapons
    armor <- armor :+ ("None", 0, 0, 0)
    ring1 <- rings :+ ("None", 0, 0, 0)
    ring2 <- rings.filter(_._1 != ring1._1) :+ ("None", 0, 0, 0)
    if !((ring1._1 == "None" && ring2._1 != "None") || (ring1._1 != "None" && ring2._1 == "None"))
  } yield {
    val cost = weapon._2 + armor._2 + ring1._2 + ring2._2
    val damage = weapon._3 + armor._3 + ring1._3 + ring2._3
    val armorValue = weapon._4 + armor._4 + ring1._4 + ring2._4

    (cost, damage, armorValue)
  }

  val losingCosts = for {
    weapon <- weapons
    armor <- armor :+ ("None", 0, 0, 0)
    ring1 <- rings :+ ("None", 0, 0, 0)
    ring2 <- rings.filter(_._1 != ring1._1) :+ ("None", 0, 0, 0)
    if !((ring1._1 == "None" && ring2._1 != "None") || (ring1._1 != "None" && ring2._1 == "None"))
  } yield {
    val cost = weapon._2 + armor._2 + ring1._2 + ring2._2
    val damage = weapon._3 + armor._3 + ring1._3 + ring2._3
    val armorValue = weapon._4 + armor._4 + ring1._4 + ring2._4

    (cost, damage, armorValue)
  }

  val minWinningCost = winningCosts.filter { case (cost, damage, armorValue) => fight(damage, armorValue) }.map(_._1).min

  val maxLosingCost = losingCosts.filter { case (cost, damage, armorValue) => !fight(damage, armorValue) }.map(_._1).max

  println(minWinningCost)
  println(maxLosingCost)
}
