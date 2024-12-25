
import "io" for File

var findWinningElf = Fn.new { |totalElves|
  var highestPowerOfTwo = 1
  while (highestPowerOfTwo * 2 <= totalElves) {
    highestPowerOfTwo = highestPowerOfTwo * 2
  }
  return (totalElves - highestPowerOfTwo) * 2 + 1
}

var totalElves = Num.fromString(File.read("input.txt").trim())
System.print(findWinningElf.call(totalElves))
