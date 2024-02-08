
object Day24 extends App {
  val packages = scala.io.Source.fromFile("input.txt").getLines().map(_.toInt).toList
  val totalWeight = packages.sum
  val groupWeight = totalWeight / 3

  def findSmallestQE(numPackages: Int): Long = {
    packages.sorted.combinations(numPackages)
      .filter(_.sum == groupWeight)
      .map(_.map(_.toLong).product)
      .min
  }

  val smallestNumPackages = Stream.from(1).find(num => packages.combinations(num).exists(_.sum == groupWeight)).get
  val smallestQE = findSmallestQE(smallestNumPackages)

  println(smallestQE)
}
