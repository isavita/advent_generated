
def coordinates = new File('input.txt').readLines().collect { it.split(',').collect { it as int } }
def maxX = coordinates.max { it[0] }[0]
def maxY = coordinates.max { it[1] }[1]

def closestCoordinate = [:]
def totalDistance = 0
def regionSize = 0

(0..maxX).each { x ->
    (0..maxY).each { y ->
        def distances = coordinates.collect { Math.abs(it[0] - x) + Math.abs(it[1] - y) }
        totalDistance = distances.sum()
        if (totalDistance < 10000) {
            regionSize++
        }
        def minDistance = distances.min()
        if (distances.count { it == minDistance } == 1) {
            def index = distances.indexOf(minDistance)
            closestCoordinate[x + y * (maxX + 1)] = index
        }
    }
}

def infiniteAreas = closestCoordinate.findAll { it.key / (maxX + 1) == 0 || it.key / (maxX + 1) == maxY || it.key % (maxX + 1) == 0 || it.key % (maxX + 1) == maxX }.keySet().collect { closestCoordinate[it] }
def finiteAreas = closestCoordinate.values.findAll { it != null && it !in infiniteAreas }
def largestFiniteArea = finiteAreas.collect { area -> finiteAreas.count { it == area } }.max()

println largestFiniteArea
println regionSize
