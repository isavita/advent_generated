
import "io" for File

var rules = {}
var memo = {}

var countBags
countBags = Fn.new {|color|
  if (memo[color]) return memo[color]
  var sum = 1
  for (child in rules[color] || []) {
    sum = sum + child[0] * countBags.call(child[1])
  }
  memo[color] = sum
  return sum
}

var lines = File.read("input.txt").split("\n")
for (line in lines) {
  if (line != "") {
    var parts = line.split(" bags contain ")
    var parent = parts[0]
    rules[parent] = []
    if (parts[1] != "no other bags.") {
      for (child in parts[1].split(", ")) {
        var words = child.split(" ")
        rules[parent].add([Num.fromString(words[0]), words[1] + " " + words[2]])
      }
    }
  }
}

System.print(countBags.call("shiny gold") - 1)
