object RocketEquation {
    def main(args: Array[String]): Unit = {
        val input = scala.io.Source.fromFile("input.txt").getLines().toList
        val totalFuel = input.map(module => {
            LazyList.iterate(module.toInt)(_ / 3 - 2).takeWhile(_ > 0).sum - module.toInt
        }).sum
        println(totalFuel)
    }
}