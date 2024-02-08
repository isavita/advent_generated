
def file = new File("input.txt")
def firewall = [:]

file.eachLine { line ->
    def fields = line.split(": ")
    def depth = fields[0] as Integer
    def rng = fields[1] as Integer
    firewall[depth] = [Range: rng, Position: 0, Direction: 1]
}

def delay = 0
while (true) {
    if (passThrough(firewall, delay)) {
        break
    }
    delay++
}

println delay

def passThrough(firewall, delay) {
    firewall.every { depth, scanner ->
        (depth + delay) % (2 * (scanner.Range - 1)) != 0
    }
}
