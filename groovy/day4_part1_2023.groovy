def totalPoints = 0
new File('input.txt').eachLine { line ->
    def (winningNumbers, myNumbers) = line.split(/\s*\|\s*/)
    def winningSet = winningNumbers.split(/\s+/) as Set
    def mySet = myNumbers.split(/\s+/) as Set
    def matches = mySet.intersect(winningSet)
    def points = matches.size() > 0 ? (1 << (matches.size() - 1)) : 0
    totalPoints += points
}

println "Total points: $totalPoints"