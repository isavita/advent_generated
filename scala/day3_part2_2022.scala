import scala.io.Source

object Main extends App {
  def itemPriority(item: Char): Int = {
    if (item >= 'a' && item <= 'z') {
      item - 'a' + 1
    } else {
      item - 'A' + 27
    }
  }

  var sum = 0
  var groupLineCounter = 0
  val groupItems = Array.fill(3)(collection.mutable.Map[Char, Int]())

  val filename = "input.txt"
  for (line <- Source.fromFile(filename).getLines) {
    val itemsMap = collection.mutable.Map[Char, Int]()
    line.foreach(item => itemsMap(item) = itemsMap.getOrElse(item, 0) + 1)
    groupItems(groupLineCounter) = itemsMap
    groupLineCounter += 1

    if (groupLineCounter == 3) {
      val commonItems = collection.mutable.Map[Char, Int]()
      for ((item, _) <- groupItems(0)) {
        if (groupItems(1).contains(item) && groupItems(2).contains(item)) {
          commonItems(item) = commonItems.getOrElse(item, 0) + 1
        }
      }
      for ((item, _) <- commonItems) {
        sum += itemPriority(item)
      }
      groupLineCounter = 0
    }
  }

  println(sum)
}