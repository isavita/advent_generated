
import scala.collection.mutable
import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val originalMap = Source.fromFile("input.txt").getLines().map(_.toList).toList

    val (modifiedMap, robotPositions) = modifyMap(originalMap)
    val (keys, doors, allKeys) = collectKeysAndDoors(modifiedMap)
    val keyPositions = (keys.keys ++ robotPositions.indices.map(i => s"@$i")).map(k =>
        if (k.startsWith("@")) k -> robotPositions(k.substring(1).toInt)
        else k -> keys(k)
    ).toMap

    val keyGraph = buildKeyGraph(modifiedMap, keyPositions, robotPositions)

    val result = dijkstra(allKeys.size, robotPositions.indices.map(i => s"@$i").toList, keyGraph)
    println(result)
  }

  def modifyMap(originalMap: List[List[Char]]): (List[List[Char]], List[(Int, Int)]) = {
    val height = originalMap.length
    val width = originalMap.head.length
    var modifiedMap = originalMap.map(_.toArray).toArray

        for (y <- 1 until height - 1; x <- 1 until width - 1) {
            if (modifiedMap(y)(x) == '@' &&
                modifiedMap(y - 1)(x) == '.' && modifiedMap(y + 1)(x) == '.' &&
                modifiedMap(y)(x - 1) == '.' && modifiedMap(y)(x + 1) == '.')
            {

                modifiedMap(y-1)(x-1) = '@'
                modifiedMap(y-1)(x) = '#'
                modifiedMap(y-1)(x+1) = '@'
                modifiedMap(y)(x-1) = '#'
                modifiedMap(y)(x) = '#'
                modifiedMap(y)(x+1) = '#'
                modifiedMap(y+1)(x-1) = '@'
                modifiedMap(y+1)(x) = '#'
                modifiedMap(y+1)(x+1) = '@'

            }
        }
    var robotPositions = List[(Int, Int)]()
        for (y <- modifiedMap.indices; x <- modifiedMap(0).indices)
            if(modifiedMap(y)(x) == '@')
                robotPositions::= (x,y)

    (modifiedMap.map(_.toList).toList, robotPositions)
  }

  def collectKeysAndDoors(map: List[List[Char]]): (Map[String, (Int, Int)], Map[Char, (Int, Int)], Set[String]) = {
    val keys = mutable.Map[String, (Int, Int)]()
    val doors = mutable.Map[Char, (Int, Int)]()
    val allKeys = mutable.Set[String]()

    for ((row, y) <- map.zipWithIndex; (cell, x) <- row.zipWithIndex) {
      if (cell.isLower) {
        keys += cell.toString -> (x, y)
        allKeys += cell.toString
      } else if (cell.isUpper) {
        doors += cell -> (x, y)
      }
    }
    (keys.toMap, doors.toMap, allKeys.toSet)
  }

    def bfs(map: List[List[Char]], startPos: (Int, Int)): Map[String, (Int, Set[String])] = {
        val queue = mutable.Queue[((Int, Int), Int, Set[String])]()
        queue.enqueue((startPos, 0, Set()))
        val visited = mutable.Set[(Int, Int)]()
        val results = mutable.Map[String, (Int, Set[String])]()

        while (queue.nonEmpty) {
            val ((x, y), dist, requiredKeys) = queue.dequeue()
            if (!visited.contains((x, y))) {
                visited.add((x, y))

                val cell = map(y)(x)
                if (cell.isLower && !requiredKeys.contains(cell.toString)) {
                    results += cell.toString -> (dist, requiredKeys)
                    val nextRequiredKeys = requiredKeys + cell.toString

                }


                for ((dx, dy) <- List((-1, 0), (1, 0), (0, -1), (0, 1))) {
                    val (nx, ny) = (x + dx, y + dy)
                    if (0 <= ny && ny < map.length && 0 <= nx && nx < map.head.length) {
                        val ncell = map(ny)(nx)
                        if (ncell != '#') {
                            if (ncell.isUpper) {
                                val nextRequiredKeys = requiredKeys + ncell.toLower.toString
                                queue.enqueue(((nx, ny), dist + 1, nextRequiredKeys))
                            } else {
                                queue.enqueue(((nx, ny), dist + 1, requiredKeys))
                            }
                        }
                    }
                }
            }
        }
        results.toMap
    }

  def buildKeyGraph(map: List[List[Char]], keyPositions: Map[String, (Int, Int)], robotPositions: List[(Int, Int)]): Map[String, Map[String, (Int, Set[String])]] = {

    val allNodes = keyPositions.keys.toList
    allNodes.map(key => key -> bfs(map,keyPositions(key))).toMap

  }

  def bitmask(keysSet: Set[String]): Int = {
    keysSet.foldLeft(0)((mask, k) => mask | (1 << (k.head - 'a')))
  }

  def dijkstra(totalKeys: Int, initialPositions: List[String], keyGraph: Map[String, Map[String, (Int, Set[String])]]): Int = {
    val initialCollectedKeys = Set[String]()
    val initialState = (initialPositions, initialCollectedKeys)
    val heap = mutable.PriorityQueue[(Int, List[String], Set[String])]((0, initialPositions, initialCollectedKeys))(Ordering.by(-_._1))
    val visited = mutable.Map[(List[String], Int), Int]()

    while (heap.nonEmpty) {
      val (cost, positions, collectedKeys) = heap.dequeue()
      val state = (positions, bitmask(collectedKeys))

      if (!visited.contains(state) || visited(state) > cost) {
        visited(state) = cost

        if (collectedKeys.size == totalKeys) {
          return cost
        }

        for (i <- positions.indices) {
          val pos = positions(i)
            if(keyGraph.contains(pos))
                for ((key, (dist, requiredKeys)) <- keyGraph(pos)) {
                  if (!collectedKeys.contains(key) && requiredKeys.subsetOf(collectedKeys)) {
                    val newPositions = positions.updated(i, key)
                    val newCollectedKeys = collectedKeys + key
                    val newState = (newPositions, bitmask(newCollectedKeys))

                    if (!visited.contains(newState) || visited(newState) > cost + dist) {
                      heap.enqueue((cost + dist, newPositions, newCollectedKeys))
                    }
                  }
                }
        }
      }
    }
    -1
  }
}
