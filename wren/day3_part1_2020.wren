import "io" for File

var countTrees = Fn.new { |forest, right, down|
  var trees = 0
  var x = 0
  var width = forest[0].count

  for (y in 0..forest.count - 1) {
    if (forest[y][x % width] == "#") {
      trees = trees + 1
    }
    x = x + right
    y = y + down
  }

  return trees
}

var main = Fn.new {
  var file = File.read("input.txt")
  var lines = file.split("
")
  var forest = []
  for (line in lines) {
    forest.add(line.trim())
  }

  var trees = countTrees.call(forest, 3, 1)
  System.print(trees)
}

main.call()