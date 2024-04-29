object SonarSweep {
  def main(args: Array[String]): Unit = {
    val input = io.Source.fromFile("input.txt").getLines().map(_.toInt)
    val measurements = input.toSeq

    // Part 1: Count the number of times a depth measurement increases from the previous measurement
    val increasedMeasurements = measurements.zip(measurements.tail).count { case (prev, curr) => curr > prev }
    println(s"Part 1: $increasedMeasurements")

    // Part 2: Count the number of times the sum of measurements in a sliding window increases from the previous sum
    val windowSums = measurements.sliding(3).map(_.sum).toSeq
    val increasedSums = windowSums.zip(windowSums.tail).count { case (prev, curr) => curr > prev }
    println(s"Part 2: $increasedSums")
  }
}