
def input = new File('input.txt').text

// Parse the input to get times and distances
def (timesLine, distancesLine) = input.split('\n')
def times = timesLine.split(':')[1].trim().split(/\s+/)*.toInteger()
def distances = distancesLine.split(':')[1].trim().split(/\s+/)*.toInteger()

// Function to calculate the number of ways to beat the record for a given race
def calculateWaysToBeatRecord(time, recordDistance) {
    def ways = 0
    for (int holdTime = 1; holdTime < time; holdTime++) {
        def speed = holdTime
        def travelTime = time - holdTime
        def distance = speed * travelTime
        if (distance > recordDistance) {
            ways++
        }
    }
    ways
}

// Calculate the number of ways to beat the record for each race
def totalWays = 1
times.eachWithIndex { time, index ->
    def recordDistance = distances[index]
    def ways = calculateWaysToBeatRecord(time, recordDistance)
    totalWays *= ways
}

// Print the result
println totalWays
