def inputFile = new File("input.txt")
def commands = inputFile.readLines()

def horizontalPosition = 0
def depth = 0

commands.each {
    def parts = it.split(" ")
    def direction = parts[0]
    def value = parts[1] as int

    if (direction == "forward") {
        horizontalPosition += value
    } else if (direction == "down") {
        depth += value
    } else if (direction == "up") {
        depth -= value
    }
}

println horizontalPosition * depth