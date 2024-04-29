def input = new File('input.txt').text
def rope = new int[10][2]
def visited = [:]

for (line in input.split('\n')) {
    def (dir, num) = line.split()
    def moveX = dir in ['R', '>'] ? 1 : dir in ['L', '<'] ? -1 : 0
    def moveY = dir in ['U', '^'] ? 1 : dir in ['D', 'v'] ? -1 : 0
    num.toInteger().times {
        rope[0][0] += moveX
        rope[0][1] += moveY
        for (int i = 1; i < 10; i++) {
            def tempX = rope[i-1][0] - rope[i][0]
            def tempY = rope[i-1][1] - rope[i][1]
            if (Math.abs(tempX) > 1 || Math.abs(tempY) > 1) {
                rope[i][0] += tempX > 0 ? 1 : tempX < 0 ? -1 : 0
                rope[i][1] += tempY > 0 ? 1 : tempY < 0 ? -1 : 0
            }
            visited["${rope[9][0]},${rope[9][1]}"] = true
        }
    }
}

println visited.size()