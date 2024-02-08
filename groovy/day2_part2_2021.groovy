
def inputFile = new File("input.txt")
def lines = inputFile.readLines()

def horizontalPosition = 0
def depth = 0
def aim = 0

lines.each { line ->
    def parts = line.split()
    def command = parts[0]
    def value = parts[1] as int

    if (command == "forward") {
        horizontalPosition += value
        depth += aim * value
    } else if (command == "down") {
        aim += value
    } else if (command == "up") {
        aim -= value
    }
}

println horizontalPosition * depth
