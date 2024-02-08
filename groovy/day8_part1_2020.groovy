def instructions = new File("input.txt").readLines()
def accumulator = 0
def index = 0
def visited = []

while (index < instructions.size()) {
    if (visited.contains(index)) {
        break
    }
    visited.add(index)
    
    def parts = instructions[index].split(" ")
    def operation = parts[0]
    def argument = parts[1] as int
    
    switch (operation) {
        case "acc":
            accumulator += argument
            index++
            break
        case "jmp":
            index += argument
            break
        case "nop":
            index++
            break
    }
}

println accumulator