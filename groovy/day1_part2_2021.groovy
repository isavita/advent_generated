
def input = new File("input.txt").readLines().collect { it.toInteger() }

def part1 = (1..<input.size()).count { input[it] > input[it - 1] }

def part2 = (2..<input.size()).count { input[it] + input[it - 1] + input[it - 2] > input[it - 1] + input[it - 2] + input[it - 3] }

println part1
println part2
