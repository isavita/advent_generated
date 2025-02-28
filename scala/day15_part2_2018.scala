import scala.collection.mutable
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

object Solution {
  val KIND_SPACE = 1
  val KIND_ELF = 2
  val KIND_GOBLIN = 4
  val KIND_WALL = 8
  val DEFAULT_HITPOINTS = 200
  val DEFAULT_POWER = 3
  val OFFSETS = List((0, -1), (-1, 0), (1, 0), (0, 1))
  val RUNE_KINDS: Map[Char, Int] = Map('.' -> KIND_SPACE, 'E' -> KIND_ELF, 'G' -> KIND_GOBLIN, '#' -> KIND_WALL)
  def isUnit(k: Int): Boolean = ((KIND_ELF | KIND_GOBLIN) & k) != 0

  class Tile(var kind: Int, val x: Int, val y: Int, val grid: mutable.Map[Int, mutable.Map[Int, Tile]]) {
    var unit: Option[UnitEntity] = None
    def walkableNeighbors: List[Tile] = {
      OFFSETS.flatMap { case (dx, dy) =>
        grid.get(y + dy).flatMap(_.get(x + dx)).filter(_.kind == KIND_SPACE).toList
      }
    }
  }

  class UnitEntity(var tile: Tile, val kind: Int, elfPower: Int) {
    var hitpoints: Int = DEFAULT_HITPOINTS
    val power: Int = if (kind == KIND_ELF) elfPower else DEFAULT_POWER
    def targets(cave: Cave): Boolean = cave.units.exists(u => u.kind != kind && u.hitpoints > 0)
    def enemies(cave: Cave): List[UnitEntity] =
      cave.units.filter(u => u.kind != kind && u.hitpoints > 0).sortBy(u => (u.tile.y, u.tile.x)).toList
    def enemyNeighbor(cave: Cave): Option[UnitEntity] = {
      var target: Option[UnitEntity] = None
      OFFSETS.foreach { case (dx, dy) =>
        cave.map.get(tile.y + dy).flatMap(_.get(tile.x + dx)).flatMap(_.unit).filter(_.kind != kind).foreach { enemy =>
          if (target.isEmpty || enemy.hitpoints < target.get.hitpoints) target = Some(enemy)
        }
      }
      target
    }
    def move(cave: Cave): Unit = {
      if (enemyNeighbor(cave).isDefined) return
      nextTile(cave)._1.foreach { next =>
        next.unit = Some(this)
        next.kind = kind
        tile.kind = KIND_SPACE
        tile.unit = None
        tile = next
      }
    }
    def nextTile(cave: Cave): (Option[Tile], Option[Tile]) = {
      val (distances, cameFrom) = findWalkableTiles(cave.map, tile)
      var targets = List.empty[Tile]
      var closest = Int.MaxValue
      for (enemy <- enemies(cave); n <- enemy.tile.walkableNeighbors) {
        distances.get(n).foreach { d =>
          if (d < closest) { closest = d; targets = List(n) }
          else if (d == closest) targets = n :: targets
        }
      }
      if (targets.nonEmpty) {
        val chosen = targets.sortBy(t => (t.y, t.x)).head
        var current = chosen
        while (cameFrom(current) != tile) { current = cameFrom(current) }
        (Some(current), Some(chosen))
      } else (None, None)
    }
    def attack(cave: Cave): Boolean = {
      enemyNeighbor(cave).map { enemy =>
        val killed = enemy.damage(cave, power)
        killed && enemy.kind == KIND_ELF
      }.getOrElse(false)
    }
    def damage(cave: Cave, dmg: Int): Boolean = {
      hitpoints -= dmg
      if (hitpoints <= 0) { cave.removeUnit(this); true } else false
    }
  }

  def findWalkableTiles(grid: mutable.Map[Int, mutable.Map[Int, Tile]], start: Tile):
    (mutable.Map[Tile, Int], mutable.Map[Tile, Tile]) = {
    val frontier = mutable.Queue[Tile](start)
    val distance = mutable.Map[Tile, Int](start -> 0)
    val cameFrom = mutable.Map[Tile, Tile]()
    while (frontier.nonEmpty) {
      val current = frontier.dequeue()
      for (n <- current.walkableNeighbors)
        if (!distance.contains(n)) { frontier.enqueue(n); distance(n) = distance(current) + 1; cameFrom(n) = current }
    }
    (distance, cameFrom)
  }

  class Cave(input: List[String], elfPower: Int) {
    val units = mutable.ArrayBuffer[UnitEntity]()
    val map = mutable.Map[Int, mutable.Map[Int, Tile]]()
    parseMap(input, elfPower)
    private def parseMap(input: List[String], elfPower: Int): Unit = {
      for ((row, y) <- input.zipWithIndex) {
        val rowMap = mutable.Map[Int, Tile]()
        for ((ch, x) <- row.zipWithIndex) {
          val kind = RUNE_KINDS.getOrElse(ch, KIND_WALL)
          val tile = new Tile(kind, x, y, map)
          if (isUnit(kind)) { val u = new UnitEntity(tile, kind, elfPower); units += u }
          rowMap(x) = tile
        }
        map(y) = rowMap
      }
    }
    def status: (Int, Boolean) = {
      var elves = false
      var goblins = false
      var hp = 0
      units.foreach { u =>
        if (u.hitpoints > 0) {
          if (u.kind == KIND_ELF) elves = true else goblins = true
          hp += u.hitpoints
        }
      }
      (hp, elves && goblins)
    }
    def removeDead(): Unit = { units --= units.filter(_.hitpoints <= 0) }
    def removeUnit(u: UnitEntity): Unit = { u.tile.kind = KIND_SPACE; u.tile.unit = None }
    def tick(stopOnElfDeath: Boolean): (Boolean, Boolean) = {
      removeDead()
      val sorted = units.sortBy(u => (u.tile.y, u.tile.x))
      for (u <- sorted if u.hitpoints > 0) {
        if (!u.targets(this)) return (false, false)
        u.move(this)
        if (u.attack(this) && stopOnElfDeath) return (false, true)
      }
      (true, false)
    }
  }

  def combat(input: List[String]): Int = {
    val cave = new Cave(input, DEFAULT_POWER)
    var rounds = 1
    while (true) {
      val (hp, cont) = cave.status
      if (!cont) return (rounds - 1) * hp
      val (clean, _) = cave.tick(false)
      if (!clean) rounds -= 1
      rounds += 1
    }
    0
  }

  def cheatingElves(input: List[String]): Int = {
    var power = 4
    while (true) {
      val cave = new Cave(input, power)
      var rounds = 1
      var elfDied = false
      breakable {
        while (true) {
          val (hp, cont) = cave.status
          if (!cont) { println((rounds - 1) * hp); sys.exit(0) }
          val (clean, died) = cave.tick(true)
          if (died) { elfDied = true; break }
          if (!clean) rounds -= 1
          rounds += 1
        }
      }
      if (elfDied) power += 1 else return (rounds - 1) * cave.status._1
    }
    0
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList.map(_.trim)
    println(cheatingElves(lines))
  }
}