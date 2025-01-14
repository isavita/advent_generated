
def raw = new File("input.txt").readLines().findAll { it.trim() }

if (raw.size() % 7 != 0) {
    println 0
    return
}

def locks = []
def keys = []

for (int i = 0; i + 7 <= raw.size(); i += 7) {
    def block = raw[i..<(i + 7)]
    if (block.any { it.size() < 5 }) continue

    if (block[0].every { it == '#' }) {
        locks << parseLock(block)
    } else {
        keys << parseKey(block)
    }
}

def count = 0
locks.each { lock ->
    keys.each { key ->
        if (fits(lock, key)) {
            count++
        }
    }
}
println count

def parseLock(block) {
    (0..<5).collect { c ->
        (1..<7).takeWhile { r -> block[r][c] == '#' }.size()
    }
}

def parseKey(block) {
    (0..<5).collect { c ->
        (0..5).reverse().takeWhile { r -> block[r][c] == '#' }.size()
    }
}

def fits(lock, key) {
    (0..<5).every { i -> lock[i] + key[i] <= 5 }
}
