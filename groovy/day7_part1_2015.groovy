
def file = new File("input.txt")
def input = file.text.trim()

def wireToRule = [:]

input.eachLine { inst ->
    def parts = inst.split(" -> ")
    wireToRule[parts[1]] = parts[0]
}

def aSignal = memoDFS(wireToRule, "a", [:])
println aSignal

int memoDFS(Map graph, String entry, Map memo) {
    def memoVal = memo[entry]
    if (memoVal != null) {
        return memoVal
    }

    if (entry.isNumber()) {
        return entry.toInteger()
    }

    def sourceRule = graph[entry]
    def parts = sourceRule.split(" ")

    def result
    switch (parts.size()) {
        case 1:
            result = memoDFS(graph, parts[0], memo)
            break
        case 2:
            switch (parts[0]) {
                case "NOT":
                    def start = memoDFS(graph, parts[1], memo)
                    result = (65535) ^ start
                    break
            }
            break
        case 3:
            switch (parts[1]) {
                case "AND":
                    result = memoDFS(graph, parts[0], memo) & memoDFS(graph, parts[2], memo)
                    break
                case "OR":
                    result = memoDFS(graph, parts[0], memo) | memoDFS(graph, parts[2], memo)
                    break
                case "LSHIFT":
                    result = memoDFS(graph, parts[0], memo) << memoDFS(graph, parts[2], memo)
                    break
                case "RSHIFT":
                    result = memoDFS(graph, parts[0], memo) >> memoDFS(graph, parts[2], memo)
                    break
            }
            break
    }

    memo[entry] = result
    return result
}
