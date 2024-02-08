
def numbers = new File("input.txt").text.tokenize(" ").collect { it as Integer }

def parseTree(data, index) {
    def childCount = data[index]
    def metaCount = data[index + 1]
    index += 2

    def childValues = []
    (0..<childCount).each { i ->
        def childValue
        def result = parseTree(data, index)
        childValue = result[0]
        index = result[1]
        childValues << childValue
    }

    def value = 0
    if (childCount == 0) {
        (0..<metaCount).each { i ->
            value += data[index + i]
        }
    } else {
        (0..<metaCount).each { i ->
            def metadata = data[index + i]
            if (metadata <= childCount && metadata > 0) {
                value += childValues[metadata - 1]
            }
        }
    }
    index += metaCount

    return [value, index]
}

def result = parseTree(numbers, 0)
println result[0]
