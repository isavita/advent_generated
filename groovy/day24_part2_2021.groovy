
class ALU {
    Map<String, Long> registers = [w: 0, x: 0, y: 0, z: 0]

    long run(List<String> program, List<Integer> inputs) {
        registers = [w: 0, x: 0, y: 0, z: 0] // Reset registers
        int inputIndex = 0
        for (String instruction : program) {
            String[] parts = instruction.split(" ")
            String op = parts[0]
            String a = parts[1]

            switch (op) {
                case "inp":
                    registers[a] = inputs[inputIndex++]
                    break
                case "add":
                    String b = parts[2]
                    registers[a] += (b.matches("-?\\d+") ? b.toLong() : registers[b])
                    break
                case "mul":
                    b = parts[2]
                    registers[a] *= (b.matches("-?\\d+") ? b.toLong() : registers[b])
                    break
                case "div":
                    b = parts[2]
                    long divisor = (b.matches("-?\\d+") ? b.toLong() : registers[b])
                    if (divisor == 0) {
                        throw new IllegalArgumentException("Division by zero")
                    }
                    registers[a] = (long) Math.floor(registers[a] / divisor)
                    break
                case "mod":
                    b = parts[2]
                    long valA = registers[a]
                    long valB = (b.matches("-?\\d+") ? b.toLong() : registers[b])
                     if (valA < 0 || valB <= 0){
                        throw new IllegalArgumentException("Invalid Modulo operation")
                     }
                    registers[a] %= valB
                    break
                case "eql":
                    b = parts[2]
                    registers[a] = (registers[a] == (b.matches("-?\\d+") ? b.toLong() : registers[b])) ? 1 : 0
                    break
            }
        }
        return registers.z
    }
}


def solve(List<String> program, boolean findLargest) {
    //Analyze the program to identify relationships.  This section heavily depends on the specific input program.
    //My input had 14 blocks, each beginning with "inp w".  The critical parts are the "div z" value,
    //and the "add x" and "add y" values after the "inp w".
    def blocks = []
    def currentBlock = []
    program.each { line ->
        if (line.startsWith("inp")) {
            if (currentBlock) {
                blocks << currentBlock
            }
            currentBlock = []
        }
        currentBlock << line
    }
    blocks << currentBlock

    def constraints = []
    blocks.eachWithIndex { block, i ->
        def divZ = block.find { it.startsWith("div z") }.split(" ")[2].toInteger()
        def addX = block.find { it.startsWith("add x") && !it.startsWith("add x z") }.split(" ")[2].toInteger()
        def addY = block.reverse().find { it.startsWith("add y") && !it.startsWith("add y w")  }.split(" ")[2].toInteger()
        constraints << [divZ, addX, addY]
    }

    // The stack and dependencies are used to determine relationships between input digits
    // based on how the z register changes through divisions and modulo operations.
    def stack = []
    def dependencies = [:]

    constraints.eachWithIndex { constraint, i ->
        def (divZ, addX, addY) = constraint
        if (divZ == 1) {
            stack.push([i, addY])
        } else {
            def (prevIndex, prevAddY) = stack.pop()
            dependencies[i] = [prevIndex, prevAddY + addX]
        }
    }
    
    def digits = new long[14]
    dependencies.each { key, value ->
        def (prevIndex, diff) = value
        if (findLargest) {
             if (diff > 0){
                digits[key] = 9
                digits[prevIndex] = 9 - diff
            } else {
                digits[prevIndex] = 9
                digits[key] = 9 + diff
            }
        } else{
           if (diff > 0){
                digits[prevIndex] = 1
                digits[key] = 1 + diff
            } else {
                digits[key] = 1
                digits[prevIndex] = 1 - diff
            }
        }       
    }
    
    stack.each { pair ->
        def (index, val) = pair
        if (findLargest){
            digits[index] = 9
        }
        else{
            digits[index] = 1
        }
    }
    return digits.join("").toLong()
}



def static main(args) {
    def file = new File("input.txt")
    def program = file.readLines()

    def alu = new ALU()

    // Part 1
    long largestModelNumber = solve(program, true)
    println "Largest model number: ${largestModelNumber}"

    //Part 2
    long smallestModelNumber = solve(program, false)
    println "Smallest model number: ${smallestModelNumber}"
}
