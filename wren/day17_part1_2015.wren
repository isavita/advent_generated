import "io" for File

class Combinations {
  static countCombinations(containers, target, index) {
    if (target == 0) return 1
    if (target < 0 || index >= containers.count) return 0
    return Combinations.countCombinations(containers, target - containers[index], index + 1) +
           Combinations.countCombinations(containers, target, index + 1)
  }
}

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")
  var containers = []

  for (line in lines) {
    var size = Num.fromString(line.trim())
    containers.add(size)
  }

  System.print(Combinations.countCombinations(containers, 150, 0))
}

main.call()