
def discs = []
new File("input.txt").eachLine { line ->
    def parts = line.split()
    discs.add([parts[3].toInteger(), parts.last().substring(0, parts.last().length() - 1).toInteger()])
}

def time = 0
def capsuleReleased = false

while (!capsuleReleased) {
    def success = true
    for (int i = 0; i < discs.size(); i++) {
        if ((discs[i][1] + i + time + 1) % discs[i][0] != 0) {
            success = false
            break
        }
    }
    if (success) {
        capsuleReleased = true
        println(time)
    }
    time++
}
