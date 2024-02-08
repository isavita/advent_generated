
def input = new File("input.txt").text.split("\n")

// Part One
def totalDiff = input.inject(0) { total, line ->
    total + (line.length() - line.replaceAll(/\\\\|\\\"|\\x[a-f0-9]{2}/, "_").length() + 2)
}
println totalDiff

// Part Two
def totalEncodedDiff = input.inject(0) { total, line ->
    total + (line.replaceAll(/\\|\"/, "__").length() + 2 - line.length())
}
println totalEncodedDiff
