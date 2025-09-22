
class IntcodeVM {
    def code = [:]
    def input = []
    def output = []
    int ip = 0
    int relativeBase = 0

    IntcodeVM(def program) {
        program.eachWithIndex { val, idx -> code[idx] = val }
    }

    def run() {
        while (true) {
            def cmd = code.getOrDefault(ip, 0)
            int opcode = cmd % 100
            def modes = [(cmd / 100).intValue() % 10, (cmd / 1000).intValue() % 10, (cmd / 10000).intValue() % 10]

            if (opcode == 1) {
                code[getAddress(modes[2], 3)] = getParam(modes[0], 1) + getParam(modes[1], 2)
                ip += 4
            } else if (opcode == 2) {
                code[getAddress(modes[2], 3)] = getParam(modes[0], 1) * getParam(modes[1], 2)
                ip += 4
            } else if (opcode == 3) {
                code[getAddress(modes[0], 1)] = input.remove(0)
                ip += 2
            } else if (opcode == 4) {
                output << getParam(modes[0], 1)
                ip += 2
            } else if (opcode == 5) {
                ip = getParam(modes[0], 1) != 0 ? getParam(modes[1], 2) : ip + 3
            } else if (opcode == 6) {
                ip = getParam(modes[0], 1) == 0 ? getParam(modes[1], 2) : ip + 3
            } else if (opcode == 7) {
                code[getAddress(modes[2], 3)] = getParam(modes[0], 1) < getParam(modes[1], 2) ? 1 : 0
                ip += 4
            } else if (opcode == 8) {
                code[getAddress(modes[2], 3)] = getParam(modes[0], 1) == getParam(modes[1], 2) ? 1 : 0
                ip += 4
            } else if (opcode == 9) {
                relativeBase += getParam(modes[0], 1)
                ip += 2
            } else if (opcode == 99) {
                break
            }
        }
        output
    }

    private int getParam(int mode, int index) {
        def addr = code.getOrDefault(ip + index, 0)
        if (mode == 0) return code.getOrDefault(addr, 0)
        if (mode == 1) return addr
        return code.getOrDefault(relativeBase + addr, 0)
    }

    private int getAddress(int mode, int index) {
        def addr = code.getOrDefault(ip + index, 0)
        mode == 0 ? addr : relativeBase + addr
    }
}

def loadProgram(filename) {
    new File(filename).text.replaceAll(/\s+/, '').split(',').collect { it as int }
}

def sendString(inputList, s) {
    s.each { inputList << (int) it }
    inputList << 10
}

def main() {
    def program = loadProgram("input.txt")
    def input = []
    def instructions = ["NOT A J", "NOT B T", "OR T J", "NOT C T", "OR T J", "AND D J", "WALK"]
    instructions.each { sendString(input, it) }
    def output = new IntcodeVM(program).with { it.input = input; it.run() }
    output.find { it > 127 }
}

println(main())
