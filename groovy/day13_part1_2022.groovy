import groovy.json.JsonSlurper

def input = new File("input.txt").text
def packets = []
def sum = 0

input.split("\n\n").eachWithIndex { pair, i ->
    def (first, second) = pair.split("\n").collect { new JsonSlurper().parseText(it) }
    packets << first << second
    if (compare(first, second) <= 0) {
        sum += i + 1
    }
}

println sum

def compare(a, b) {
    if (a instanceof Number && b instanceof Number) {
        return a <=> b
    } else if (a instanceof Number) {
        return compare([a], b)
    } else if (b instanceof Number) {
        return compare(a, [b])
    } else {
        def maxLength = Math.max(a.size(), b.size())
        for (int i = 0; i < maxLength; i++) {
            if (i >= a.size()) {
                return -1
            } else if (i >= b.size()) {
                return 1
            } else {
                def c = compare(a[i], b[i])
                if (c != 0) {
                    return c
                }
            }
        }
        return a.size() <=> b.size()
    }
}