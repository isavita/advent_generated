
def layers = []
def maxDepth = 0

new File("input.txt").eachLine { line ->
    def parts = line.split(": ")
    def depth = parts[0] as int
    def range = parts[1] as int
    layers[depth] = range
    maxDepth = Math.max(maxDepth, depth)
}

def severity = 0
for (int i = 0; i <= maxDepth; i++) {
    if (layers[i] != null && i % (2 * (layers[i] - 1)) == 0) {
        severity += i * layers[i]
    }
}

println severity
