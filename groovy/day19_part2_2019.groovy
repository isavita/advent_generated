
import java.nio.file.Files
import java.nio.file.Paths
import java.util.HashMap

class VM {
    Map<Integer, Integer> code
    int ip = 0
    int relativeBase = 0

    private List<Integer> input = []
    private List<Integer> output = []

    VM(Map<Integer, Integer> initialCode) {
        this.code = new HashMap<>(initialCode)
    }

    void setInput(int value) {
        input << value
    }

    int getOutput() {
        output.remove(0)
    }

    private int readMemory(int address) {
        code.get(address) ?: 0
    }

    void run() {
        while (true) {
            def instruction = readMemory(ip)
            def opcode = instruction % 100
            def modes = instruction.intdiv(100)

            int arity
            def params = []

            switch (opcode) {
                case 1:
                case 2:
                case 7:
                case 8:
                    arity = 3
                    params = getParamsAddresses(ip, modes, arity)
                    def val1 = readMemory(params[0])
                    def val2 = readMemory(params[1])
                    def dest = params[2]
                    if (opcode == 1) {
                        code[dest] = val1 + val2
                    } else if (opcode == 2) {
                        code[dest] = val1 * val2
                    } else if (opcode == 7) {
                         code[dest] = (val1 < val2) ? 1 : 0
                    } else {
                         code[dest] = (val1 == val2) ? 1 : 0
                    }
                    break

                case 3:
                    arity = 1
                    params = getParamsAddresses(ip, modes, arity)
                    code[params[0]] = input.remove(0)
                    break

                case 4:
                    arity = 1
                    params = getParamsAddresses(ip, modes, arity)
                    output << readMemory(params[0])
                    break

                case 5:
                case 6:
                    arity = 2
                    params = getParamsAddresses(ip, modes, arity)
                    def val1 = readMemory(params[0])
                    def val2 = readMemory(params[1])

                    def shouldJump = false
                    if (opcode == 5 && val1 != 0) {
                        shouldJump = true
                    } else if (opcode == 6 && val1 == 0) {
                        shouldJump = true
                    }

                    if (shouldJump) {
                        ip = val2
                        continue
                    }
                    break

                case 9:
                    arity = 1
                    params = getParamsAddresses(ip, modes, arity)
                    relativeBase += readMemory(params[0])
                    break

                case 99:
                    return

                default:
                    throw new IllegalArgumentException("Unknown opcode: $opcode at position $ip")
            }

            ip += arity + 1
        }
    }

    private List<Integer> getParamsAddresses(int pos, int modes, int arity) {
        List<Integer> addresses = []
        for (int i = 0; i < arity; i++) {
            def divisor = (int)Math.pow(10, i)
            def mode = modes.intdiv(divisor) % 10
            addresses << getParamAddress(pos + i + 1, mode)
        }
        return addresses
    }

    private int getParamAddress(int paramPos, int mode) {
        switch (mode) {
            case 0:
                return readMemory(paramPos)
            case 1:
                return paramPos
            case 2:
                return relativeBase + readMemory(paramPos)
            default:
                throw new IllegalArgumentException("Unknown parameter mode: $mode")
        }
    }
}

boolean beam(Map<Integer, Integer> initialCode, int x, int y) {
    def vm = new VM(initialCode)
    vm.setInput(x)
    vm.setInput(y)
    vm.run()
    def output = vm.getOutput()
    return output == 1
}

static void main(String[] args) {
    def initialCode = [:]
    def content = new String(Files.readAllBytes(Paths.get("input.txt")))
    content.trim().split(',').eachWithIndex { String s, int i ->
        initialCode[i] = s.toInteger()
    }

    int y = 20
    int x = 0

    while (true) {
        while (!beam(initialCode, x, y)) {
            x++
        }

        if (!beam(initialCode, x + 99, y)) {
            y++
            continue
        }

        if (!beam(initialCode, x, y + 99)) {
            x++
            continue
        }

        println(x * 10000 + y)
        return
    }
}
