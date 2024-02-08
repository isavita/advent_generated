
def file = new File("input.txt")
def visited = [:]
def head = [x: 0, y: 0]
def tail = [x: 0, y: 0]
visited[tail.toString()] = true

file.eachLine { line ->
    def parts = line.split(" ")
    def dir = parts[0]
    def steps = parts[1].toInteger()

    (1..steps).each {
        switch (dir) {
            case "R":
                head.x++
                break
            case "L":
                head.x--
                break
            case "U":
                head.y++
                break
            case "D":
                head.y--
                break
        }

        if (Math.abs(head.x - tail.x) > 1 || Math.abs(head.y - tail.y) > 1) {
            if (head.x != tail.x && head.y != tail.y) {
                if (head.x > tail.x) {
                    tail.x++
                } else {
                    tail.x--
                }
                if (head.y > tail.y) {
                    tail.y++
                } else {
                    tail.y--
                }
            } else {
                if (head.x > tail.x) {
                    tail.x++
                } else if (head.x < tail.x) {
                    tail.x--
                }
                if (head.y > tail.y) {
                    tail.y++
                } else if (head.y < tail.y) {
                    tail.y--
                }
            }
        }

        visited[tail.toString()] = true
    }
}

println visited.size()
