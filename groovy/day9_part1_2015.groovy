
def distances = [:]
new File('input.txt').eachLine { line ->
    def parts = line.split(' ')
    distances["${parts[0]}-${parts[2]}"] = parts[4] as int
    distances["${parts[2]}-${parts[0]}"] = parts[4] as int
}

def cities = distances.keySet().collect { it.split('-')[0] }.unique()

def permute(list) {
    if (list.size() == 1) {
        return [list]
    } else {
        def result = []
        list.each { item ->
            def rest = list - item
            permute(rest).each { subList ->
                result << ([item] + subList)
            }
        }
        return result
    }
}

def minDistance = Integer.MAX_VALUE
permute(cities).each { perm ->
    def distance = 0
    for (int i = 0; i < perm.size() - 1; i++) {
        distance += distances["${perm[i]}-${perm[i + 1]}"]
    }
    if (distance < minDistance) {
        minDistance = distance
    }
}

println minDistance
