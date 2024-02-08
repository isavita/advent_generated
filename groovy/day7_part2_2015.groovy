
def input = new File("input.txt").text.trim()

def wireToRule = [:]
input.readLines().each {
    def parts = it.split(" -> ")
    wireToRule[parts[1]] = parts[0]
}

def aSignal = memoDFS(wireToRule, "a", [:])

wireToRule["b"] = aSignal.toString()
println memoDFS(wireToRule, "a", [:])

def memoDFS(graph, entry, memo) {
    if (memo.containsKey(entry)) {
        return memo[entry]
    }

    if (entry.isNumber()) {
        return entry.toInteger()
    }

    def sourceRule = graph[entry]
    def parts = sourceRule.split("\\s+")

    def result
    switch (parts.size()) {
        case 1:
            result = memoDFS(graph, parts[0], memo)
            break
        case 2:
            result = (65535 ^ memoDFS(graph, parts[1], memo))
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
    }

    memo[entry] = result
    return result
}

String.metaClass.isNumber = {
    delegate.matches("\\d+")
}

String.metaClass.toInteger = {
    delegate.toInteger()
}
