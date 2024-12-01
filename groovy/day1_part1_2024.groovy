def input = new File('input.txt').readLines().findAll { it.trim() }

def (leftList, rightList) = input.collect { line ->
    def nums = line.trim().split(/\s+/).collect { it.toInteger() }
    [nums.first(), nums.last()]
}.transpose()

def totalDistance = [leftList, rightList].collect { it.sort() }
    .transpose()
    .sum { left, right -> Math.abs(left - right) }

println "Total distance: $totalDistance"
