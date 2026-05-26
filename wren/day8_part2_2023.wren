
import "io" for File

var gcd = Fn.new { |a, b|
  while (b != 0) {
    var temp = b
    b = a % b
    a = temp
  }
  return a
}

var lcm = Fn.new { |a, b| ((a * b) / gcd.call(a, b)).floor }

var lines = File.read("input.txt").split("\n")
for (i in 0...lines.count) {
  if (lines[i].endsWith("\r")) {
    lines[i] = lines[i][0...-1]
  }
}

while (lines.count > 0 && lines[-1] == "") {
  lines.removeAt(lines.count - 1)
}

var instructions = lines[0]
var instLen = instructions.count

var nodes = {}
var starts = []

for (i in 2...lines.count) {
  var line = lines[i]
  if (line == "") continue
  var parts = line.split(" = ")
  var head = parts[0]
  var targets = parts[1][1...parts[1].count-1].split(", ")
  nodes[head] = targets
  if (head.endsWith("A")) {
    starts.add(head)
  }
}

var steps = []
for (start in starts) {
  var curr = start
  var step = 0
  while (!curr.endsWith("Z")) {
    var inst = instructions[step % instLen]
    var choice = (inst == "L") ? 0 : 1
    curr = nodes[curr][choice]
    step = step + 1
  }
  steps.add(step)
}

var ans = steps[0]
for (i in 1...steps.count) {
  ans = lcm.call(ans, steps[i])
}

System.print(ans)
