def count = 0
new File('input.txt').eachLine { line ->
    def sides = line.tokenize().collect { it as int }
    if (sides[0] + sides[1] > sides[2] && sides[0] + sides[2] > sides[1] && sides[1] + sides[2] > sides[0]) {
        count++
    }
}
println count