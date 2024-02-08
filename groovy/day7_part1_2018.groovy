def input = new File("input.txt").readLines()

def graph = [:].withDefault { [] }
def inDegrees = [:].withDefault { 0 }

input.each {
    def parts = it.split(" ")
    def step1 = parts[1]
    def step2 = parts[7]
    
    graph[step1] += step2
    inDegrees[step2]++
}

def result = ""
def available = graph.findAll { k, v -> inDegrees[k] == 0 }.keySet().toList().sort()

while (available) {
    def step = available.remove(0)
    result += step
    
    graph[step].each { s ->
        inDegrees[s]--
        if (inDegrees[s] == 0) {
            available.add(s)
            available.sort()
        }
    }
}

println result