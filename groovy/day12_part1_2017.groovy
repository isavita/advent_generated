def input = new File("input.txt").readLines()
def graph = input.collectEntries { line ->
    def parts = line.split("<->")
    def key = parts[0].trim() as Integer
    def values = parts[1].split(",").collect { it.trim() as Integer }
    [key, values]
}

def visited = new HashSet<Integer>()
def queue = [0]

while (queue) {
    def current = queue.remove(0)
    visited.add(current)
    graph[current].each { neighbor ->
        if (!visited.contains(neighbor) && !queue.contains(neighbor)) {
            queue.add(neighbor)
        }
    }
}

println visited.size()