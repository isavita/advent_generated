
def checksum = 0
new File("input.txt").eachLine { line ->
    def nums = line.tokenize().collect { it as int }
    checksum += nums.max() - nums.min()
}
println checksum
