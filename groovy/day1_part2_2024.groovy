def input = new File('input.txt').readLines().findAll { it.trim() }

def (leftList, rightList) = input.collect { line ->
    def nums = line.trim().split(/\s+/).collect { it.toInteger() }
    [nums.first(), nums.last()]
}.transpose()

def rightCounts = rightList.countBy { it }
def similarityScore = leftList.sum { num -> num * (rightCounts[num] ?: 0) }

println "Similarity score: $similarityScore"
