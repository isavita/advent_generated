
def numbers = new File("input.txt").text.tokenize().collect { it as int }
def (sum, _) = parseTree(numbers, 0)
println sum

def parseTree(data, index) {
    def childCount = data[index]
    def metaCount = data[index + 1]
    index += 2
    def sum = 0

    (0..<childCount).each {
        def (childSum, newIndex) = parseTree(data, index)
        sum += childSum
        index = newIndex
    }

    (0..<metaCount).each {
        sum += data[index + it]
    }
    index += metaCount

    [sum, index]
}
