
object Day1 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList.map(_.toInt)
  
  // Part One
  val partOneResult = input.sum
  println(partOneResult)
  
  // Part Two
  var frequencySet = Set(0)
  var currentFrequency = 0
  var foundDuplicate = false
  var i = 0
  
  while (!foundDuplicate) {
    currentFrequency += input(i)
    if (frequencySet.contains(currentFrequency)) {
      println(currentFrequency)
      foundDuplicate = true
    } else {
      frequencySet += currentFrequency
      i = (i + 1) % input.length
    }
  }
}
