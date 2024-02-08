
def cubes = [:]
def neighbors = [
    [-1, 0, 0],
    [1, 0, 0],
    [0, -1, 0],
    [0, 1, 0],
    [0, 0, -1],
    [0, 0, 1]
]
def min = [Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE]
def max = [Integer.MIN_VALUE, Integer.MIN_VALUE, Integer.MIN_VALUE]

new File("input.txt").eachLine { line ->
    if (line.trim() == "") return
    def cube = line.tokenize(',').collect { it as Integer }
    cubes[cube] = true
    min = [Math.min(min[0], cube[0]), Math.min(min[1], cube[1]), Math.min(min[2], cube[2])]
    max = [Math.max(max[0], cube[0]), Math.max(max[1], cube[1]), Math.max(max[2], cube[2])]
}
min = min.collect { it - 1 }
max = max.collect { it + 1 }

def faces = 0
def q = [min]
def seen = [min: true]
while (q) {
    def curr = q.remove(0)
    neighbors.each { delta ->
        def next = [(curr[0] + delta[0]), (curr[1] + delta[1]), (curr[2] + delta[2])]
        if (next[0] < min[0] || next[1] < min[1] || next[2] < min[2] || next[0] > max[0] || next[1] > max[1] || next[2] > max[2]) {
            return
        }
        if (cubes.containsKey(next)) {
            faces++
        } else if (!seen.containsKey(next)) {
            seen[next] = true
            q.add(next)
        }
    }
}
println faces
