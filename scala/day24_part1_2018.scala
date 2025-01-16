
import scala.io.Source
import scala.util.matching.Regex

object Solution {

  case class Group(
      var units: Int,
      hitPoints: Int,
      attackDamage: Int,
      attackType: String,
      initiative: Int,
      immunities: Seq[String] = Seq.empty,
      weaknesses: Seq[String] = Seq.empty,
      var attacker: Option[Group] = None,
      var target: Option[Group] = None
  ) {
    def effectivePower: Int = units * attackDamage

    def damageDealt(e: Group): Int = {
      if (e.immunities.contains(attackType)) 0
      else if (e.weaknesses.contains(attackType)) effectivePower * 2
      else effectivePower
    }
  }

  type Army = Seq[Group]
  type Battlefield = Map[Int, Army]

  val ArmyImmuneSystem = 1
  val ArmyInfection = 2

  val armyName: Regex = """^(.*):$""".r
  val groupImmunities: Regex = """immune to (.*?)[;)]""".r
  val groupWeaknesses: Regex = """weak to (.*?)[;)]""".r
  val groupDescription: Regex =
    """^(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)$""".r

  def prepareForBattle(input: Seq[String]): (Battlefield, Seq[Group]) = {
    var initiative: Seq[Group] = Seq.empty
    var battle: Battlefield = Map.empty
    var currentArmy: Int = 0

    for (line <- input) {
      line match {
        case armyName(army) =>
          currentArmy = army match {
            case "Immune System" => ArmyImmuneSystem
            case "Infection"     => ArmyInfection
            case _               => throw new Exception(s"Unknown army: $army")
          }
        case groupDescription(count, hp, damage, damageType, init) =>
          val group = Group(
            units = count.toInt,
            hitPoints = hp.toInt,
            attackDamage = damage.toInt,
            attackType = damageType,
            initiative = init.toInt,
            immunities = groupImmunities.findFirstMatchIn(line).map(_.group(1).split(", ").toSeq).getOrElse(Seq.empty),
            weaknesses = groupWeaknesses.findFirstMatchIn(line).map(_.group(1).split(", ").toSeq).getOrElse(Seq.empty)
          )
          battle = battle.updated(currentArmy, battle.getOrElse(currentArmy, Seq.empty) :+ group)
          initiative :+= group
        case _ =>
      }
    }
    (battle, initiative)
  }

  def findTargets(battle: Battlefield): Unit = {
    for ((army, groups) <- battle; group <- groups.sortBy(g => (-g.effectivePower, -g.initiative))) {
      var mostDamage = 0
      var targetGroup: Option[Group] = None

      for ((enemyArmy, enemyGroups) <- battle; if army != enemyArmy; enemyGroup <- enemyGroups) {
        if (enemyGroup.units > 0 && enemyGroup.attacker.isEmpty && group.damageDealt(enemyGroup) > 0) {
          val damage = group.damageDealt(enemyGroup)
          if (damage > mostDamage) {
            mostDamage = damage
            targetGroup = Some(enemyGroup)
          } else if (damage == mostDamage) {
            targetGroup = targetGroup.flatMap { tg =>
              if (enemyGroup.effectivePower > tg.effectivePower) Some(enemyGroup)
              else if (enemyGroup.effectivePower == tg.effectivePower && enemyGroup.initiative > tg.initiative)
                Some(enemyGroup)
              else Some(tg)
            }
          }
        }
      }
      targetGroup.foreach { tg =>
        group.target = Some(tg)
        tg.attacker = Some(group)
      }
    }
  }

  def attack(initiative: Seq[Group]): Unit = {
    for (group <- initiative.sortBy(_.initiative).reverse) {
      if (group.units > 0) {
        group.target.foreach { target =>
          if (target.units > 0) {
            target.units -= group.damageDealt(target) / target.hitPoints
          }
        }
      }
      group.target.foreach { target =>
        target.attacker = None
        group.target = None
      }
    }
  }

  def clean(battle: Battlefield, initiative: Seq[Group]): (Battlefield, Seq[Group]) = {
    val newBattle = battle.map { case (army, groups) =>
      army -> groups.filter(_.units > 0)
    }
    val newInitiative = initiative.filter(_.units > 0).sortBy(_.initiative).reverse
    (newBattle, newInitiative)
  }

  def active(battle: Battlefield): Boolean = {
    battle.values.forall(_.exists(_.units > 0))
  }

  def result(battle: Battlefield): (Int, Int) = {
    var winner = 0
    var units = 0

    for ((army, groups) <- battle; if groups.exists(_.units > 0)) {
      winner = army
      units = groups.map(_.units).sum
    }
    (winner, units)
  }

  def conditionFight(input: Seq[String]): Int = {
    var (battle, initiative) = prepareForBattle(input)

    while (active(battle)) {
      findTargets(battle)
      attack(initiative)
      val (newBattle, newInitiative) = clean(battle, initiative)
      battle = newBattle
      initiative = newInitiative
    }
    result(battle)._2
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().toSeq
    println(conditionFight(input))
  }
}
