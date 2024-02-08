
def instructions = new File("input.txt").readLines()

def executeProgram(instructions) {
    def executedInstructions = []
    def accumulator = 0
    def index = 0

    while (index < instructions.size()) {
        if (executedInstructions.contains(index)) {
            return [false, accumulator]
        }

        executedInstructions.add(index)

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

    return [true, accumulator]
}

def result = executeProgram(instructions)
println result[1]

for (int i = 0; i < instructions.size(); i++) {
    def modifiedInstructions = instructions.collect { it }
    if (modifiedInstructions[i].startsWith("jmp")) {
        modifiedInstructions[i] = modifiedInstructions[i].replace("jmp", "nop")
    } else if (modifiedInstructions[i].startsWith("nop")) {
        modifiedInstructions[i] = modifiedInstructions[i].replace("nop", "jmp")
    }

    def modifiedResult = executeProgram(modifiedInstructions)
    if (modifiedResult[0]) {
        println modifiedResult[1]
        break
    }
}
