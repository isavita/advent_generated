def input = new File("input.txt").readLines()

def distances = [:].withDefault{ [:].withDefault{ 0 } }

input.each { line ->
    def parts = line.split(" ")
    def location1 = parts[0]
    def location2 = parts[2]
    def distance = parts[4] as int

    distances[location1][location2] = distance
    distances[location2][location1] = distance
}

def locations = distances.keySet().toList()

def permute(list) {
    if (list.size() == 1) {
        return [list]
    } else {
        def result = []
        list.each { item ->
            def rest = list - [item]
            permute(rest).each { subList ->
                result << ([item] + subList)
            }
        }
        return result
    }
}

def minDistance = Integer.MAX_VALUE
def maxDistance = 0

permute(locations).each { route ->
    def distance = 0
    for (int i = 0; i < route.size() - 1; i++) {
        distance += distances[route[i]][route[i + 1]]
    }
    minDistance = Math.min(minDistance, distance)
    maxDistance = Math.max(maxDistance, distance)
}

println(minDistance)
println(maxDistance)