
object SnafuConverter {
  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines = scala.io.Source.fromFile(filename).getLines().toList
    
    // Convert each SNAFU number to decimal and sum
    val decimalSum = lines.map(snafuToDecimal).sum
    
    // Convert decimal sum back to SNAFU
    val result = decimalToSnafu(decimalSum)
    
    println(result)
  }
  
  // Convert SNAFU to decimal
  def snafuToDecimal(snafu: String): Long = {
    snafu.reverse.zipWithIndex.map { case (char, index) =>
      val value = char match {
        case '2' => 2
        case '1' => 1
        case '0' => 0
        case '-' => -1
        case '=' => -2
      }
      value * math.pow(5, index).toLong
    }.sum
  }
  
  // Convert decimal to SNAFU
  def decimalToSnafu(decimal: Long): String = {
    def convert(n: Long): String = {
      if (n == 0) ""
      else {
        val remainder = n % 5
        val quotient = n / 5
        
        remainder match {
          case 0 => convert(quotient) + "0"
          case 1 => convert(quotient) + "1"
          case 2 => convert(quotient) + "2"
          case 3 => convert(quotient + 1) + "="
          case 4 => convert(quotient + 1) + "-"
        }
      }
    }
    
    convert(decimal)
  }
}
