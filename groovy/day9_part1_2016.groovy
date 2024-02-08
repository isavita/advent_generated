def input = new File("input.txt").text.trim()

def decompressedLength = 0
def index = 0

while (index < input.size()) {
    if (input[index] == '(') {
        def markerEnd = input.indexOf(')', index)
        def (length, repeat) = input.substring(index + 1, markerEnd).tokenize('x')*.toInteger()
        index = markerEnd + 1
        def data = input.substring(index, index + length)
        decompressedLength += data.size() * repeat
        index += length
    } else {
        decompressedLength++
        index++
    }
}

println decompressedLength