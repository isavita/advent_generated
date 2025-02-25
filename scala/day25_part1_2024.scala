
object LockKey {
  def parseLock(b: List[String]): List[Int] = {
    (0 until 5).map { c =>
      (1 until 7).takeWhile(r => b(r)(c) == '#').length
    }.toList
  }

  def parseKey(b: List[String]): List[Int] = {
    (0 until 5).map { c =>
      (5 to 0 by -1).takeWhile(r => b(r)(c) == '#').length
    }.toList
  }

  def fits(lock: List[Int], key: List[Int]): Boolean = {
    lock.zip(key).forall { case (l, k) => l + k <= 5 }
  }

  def main(args: Array[String]): Unit = {
    val raw = scala.io.Source.fromFile("input.txt").getLines().filter(_.trim.nonEmpty).toList
    if (raw.length % 7 != 0) {
      println(0)
      return
    }

    val (locks, keys) = raw.grouped(7).foldLeft((List[List[Int]](), List[List[Int]]())) {
      case ((ls, ks), block) =>
        if (block.exists(_.length < 5)) {
          (ls, ks)
        } else if (block.head.forall(_ == '#')) {
          (parseLock(block) :: ls, ks)
        } else {
          (ls, parseKey(block) :: ks)
        }
    }
        val count = locks.flatMap(lock => keys.map(key => fits(lock,key))).count(identity)
    println(count)
  }
}
