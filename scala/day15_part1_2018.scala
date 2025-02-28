import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue

object Main {
  val KIND_SPACE = 1 << 0
  val KIND_ELF = 1 << 1
  val KIND_GOBLIN = 1 << 2
  val KIND_WALL  = 1 << 3
  val DEFAULT_HITPOINTS = 200
  val DEFAULT_POWER = 3
  val RUNE_KINDS = Map('.' -> KIND_SPACE, 'E' -> KIND_ELF, 'G' -> KIND_GOBLIN, '#' -> KIND_WALL)
  val OFFSETS = List((0, -1), (-1, 0), (1, 0), (0, 1))
  
  class Tile(var kind: Int, val x: Int, val y: Int, val mapRef: mutable.Map[Int, mutable.Map[Int, Tile]]) {
    var unit: Option[UnitEntity] = None
    def walkableNeighbors: List[Tile] =
      OFFSETS.flatMap { case (dx, dy) =>
        mapRef.get(y + dy).flatMap(_.get(x + dx)).filter(_.kind == KIND_SPACE)
      }
  }
  
  class UnitEntity(var tile: Tile, val kind: Int, elfPower: Int, val cave: Cave) {
    var hitpoints: Int = DEFAULT_HITPOINTS
    val power: Int = if(kind == KIND_ELF) elfPower else DEFAULT_POWER
    def targets: Boolean = cave.units.exists(u => u.kind != kind && u.hitpoints > 0)
    def enemies: List[UnitEntity] =
      cave.units.filter(u => u.kind != kind && u.hitpoints > 0).sortBy(u => (u.tile.y, u.tile.x)).toList
    def enemyNeighbor: Option[UnitEntity] =
      OFFSETS.flatMap { case (dx, dy) =>
        cave.map.get(tile.y + dy).flatMap(_.get(tile.x + dx)).flatMap(_.unit)
          .filter(u => u.kind != kind && u.hitpoints > 0)
      }.sortBy(_.hitpoints).headOption
    def nextTile: Option[(Tile, Tile)] = {
      val (distances, cameFrom) = Cave.findWalkableTiles(cave.map, tile)
      var targetsList = List.empty[Tile]
      var closestDist = Int.MaxValue
      for(enemy <- enemies; neighbor <- enemy.tile.walkableNeighbors) {
        distances.get(neighbor) match {
          case Some(d) if d < closestDist =>
            closestDist = d
            targetsList = List(neighbor)
          case Some(d) if d == closestDist =>
            targetsList ::= neighbor
          case _ =>
        }
      }
      if (targetsList.nonEmpty) {
        val target = targetsList.sortBy(t => (t.y, t.x)).head
        var current = target
        while (cameFrom(current) != tile) current = cameFrom(current)
        Some((current, target))
      } else None
    }
    def move(): Unit = {
      if (enemyNeighbor.isDefined) return
      nextTile.foreach { case (nt, _) =>
        nt.unit = Some(this)
        nt.kind = kind
        tile.kind = KIND_SPACE
        tile.unit = None
        tile = nt
      }
    }
    def attack(): Boolean = {
      enemyNeighbor match {
        case Some(enemy) =>
          val killed = enemy.damage(power)
          if (killed && enemy.kind == KIND_ELF) true else false
        case None => false
      }
    }
    def damage(dmg: Int): Boolean = {
      hitpoints -= dmg
      if (hitpoints <= 0) { cave.removeUnit(this); true } else false
    }
  }
  
  class Cave(input: List[String], elfPower: Int) {
    val units = ArrayBuffer.empty[UnitEntity]
    val map = mutable.Map.empty[Int, mutable.Map[Int, Tile]]
    parseMap(input, elfPower)
    def parseMap(input: List[String], elfPower: Int): Unit = {
      for ((row, y) <- input.zipWithIndex) {
        val rowMap = mutable.Map.empty[Int, Tile]
        map(y) = rowMap
        for ((col, x) <- row.zipWithIndex) {
          val kind = RUNE_KINDS.getOrElse(col, KIND_WALL)
          val tile = new Tile(kind, x, y, map)
          if ((kind & (KIND_ELF | KIND_GOBLIN)) != 0) {
            val unit = new UnitEntity(tile, kind, elfPower, this)
            units.append(unit)
          }
          rowMap(x) = tile
        }
      }
    }
    def status: (Int, Boolean) = {
      var hp = 0
      var elves = false
      var goblins = false
      units.foreach { u =>
        if (u.hitpoints > 0) {
          if(u.kind == KIND_ELF) elves = true else goblins = true
          hp += u.hitpoints
        }
      }
      (hp, elves && goblins)
    }
    def removeTheDead(): Unit = { units --= units.filter(_.hitpoints <= 0) }
    def removeUnit(u: UnitEntity): Unit = { u.tile.kind = KIND_SPACE; u.tile.unit = None; u.tile = null }
    def tick(stopOnElfDeath: Boolean): (Boolean, Boolean) = {
      removeTheDead()
      val sortedUnits = units.sortBy(u => (u.tile.y, u.tile.x))
      for (u <- sortedUnits) {
        if (u.hitpoints <= 0) {}
        else if (!u.targets) return (false, false)
        else {
          u.move()
          if(u.attack() && stopOnElfDeath) return (false, true)
        }
      }
      (true, false)
    }
  }
  
  object Cave {
    def findWalkableTiles(map: mutable.Map[Int, mutable.Map[Int, Tile]], start: Tile):
      (mutable.Map[Tile, Int], mutable.Map[Tile, Tile]) = {
      val frontier = Queue(start)
      val distance = mutable.Map(start -> 0)
      val cameFrom = mutable.Map[Tile, Tile](start -> start)
      while(frontier.nonEmpty) {
        val current = frontier.dequeue()
        for(neighbor <- current.walkableNeighbors if !distance.contains(neighbor)) {
          frontier.enqueue(neighbor)
          distance(neighbor) = distance(current) + 1
          cameFrom(neighbor) = current
        }
      }
      (distance, cameFrom)
    }
  }
  
  def combat(input: List[String]): Int = {
    val cave = new Cave(input, DEFAULT_POWER)
    var i = 1
    while (true) {
      val (hp, cont) = cave.status
      if (!cont) return (i - 1) * hp
      val (cleanRound, _) = cave.tick(stopOnElfDeath = false)
      if(!cleanRound) i -= 1
      i += 1
    }
    -1
  }
  
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().map(_.stripLineEnd).toList
    println(combat(input))
  }
}