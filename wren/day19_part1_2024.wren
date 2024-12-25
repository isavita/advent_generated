
import "io" for File

var solve = Fn.new { |input|
  var lines = input.split("\n")
  var patterns = lines[0].split(", ").map { |s| s.trim() }
  var designs = lines.skip(2).where { |line| !line.isEmpty }.toList
  
  var possibleCount = 0
  for (design in designs) {
    var dp = List.filled(design.count + 1, false)
    dp[0] = true
    
    for (i in 0...design.count) {
      if (dp[i]) {
        for (pattern in patterns) {
          if (i + pattern.count <= design.count && design[i...i + pattern.count] == pattern) {
            dp[i + pattern.count] = true
          }
        }
      }
    }
    if (dp[design.count]) {
      possibleCount = possibleCount + 1
    }
  }
  return possibleCount
}

var input = File.read("input.txt")
var result = solve.call(input)
System.print(result)
