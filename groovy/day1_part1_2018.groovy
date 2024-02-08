def input = new File("input.txt").readLines()
def result = input.collect { it as int }.sum()
println result