
def lines = new File('input.txt').readLines()

def part1 = ''
def part2 = ''

(0..<lines[0].size()).each { i ->
    def charCount = [:].withDefault{0}
    lines.each { line ->
        charCount[line[i]]++
    }
    
    part1 += charCount.max{it.value}.key
    part2 += charCount.min{it.value}.key
}

println part1
println part2
