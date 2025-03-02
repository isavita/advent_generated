
class VM {
    Map<Long, Long> code = [:]
    long ip = 0
    List<Long> input = []
    List<Long> output = []
    long relativeBase = 0

    VM(String filename) {
        load(filename)
    }

    void load(String filename) {
        new File(filename).text.trim().split(',').eachWithIndex { String item, int i ->
            code[(long) i] = item.toLong()
        }
    }

    void run() {
        loop:
        while (true) {
            long cmd = code.getOrDefault(ip, 0L)
            long opcode = cmd % 100
            List<Long> modes = (0..2).collect { (cmd / (10 ** (it + 2))) % 10 }

            def getParam = { int index ->
                long mode = modes[index - 1]
                long val = code.getOrDefault(ip + index, 0L)
                if (mode == 0) {
                    return code.getOrDefault(val, 0L)
                } else if (mode == 1) {
                    return val
                } else { // mode == 2
                    return code.getOrDefault(relativeBase + val, 0L)
                }
            }

            def getAddress = { int index ->
                long mode = modes[index - 1]
                long val = code.getOrDefault(ip + index, 0L)
                if (mode == 0) {
                    return val
                } else { // mode == 2
                    return relativeBase + val
                }
            }

            switch (opcode) {
                case 1: // add
                    code[getAddress(3)] = getParam(1) + getParam(2)
                    ip += 4
                    break
                case 2: // multiply
                    code[getAddress(3)] = getParam(1) * getParam(2)
                    ip += 4
                    break
                case 3: // input
                    code[getAddress(1)] = input.remove(0)
                    ip += 2
                    break
                case 4: // output
                    output << getParam(1)
                    ip += 2
                    break
                case 5: // jump-if-true
                    if (getParam(1) != 0) {
                        ip = getParam(2)
                    } else {
                        ip += 3
                    }
                    break
                case 6: // jump-if-false
                    if (getParam(1) == 0) {
                        ip = getParam(2)
                    } else {
                        ip += 3
                    }
                    break
                case 7: // less than
                    code[getAddress(3)] = (getParam(1) < getParam(2)) ? 1 : 0
                    ip += 4
                    break
                case 8: // equals
                    code[getAddress(3)] = (getParam(1) == getParam(2)) ? 1 : 0
                    ip += 4
                    break
                case 9: // adjust relative base
                    relativeBase += getParam(1)
                    ip += 2
                    break
                case 99: // halt
                    break loop
                default:
                    throw new Exception("Unknown opcode ${opcode}")
            }
        }
    }
}

long beam(long x, long y, String filename) {
    VM vm = new VM(filename)
    vm.input = [x, y]
    vm.run()
    return vm.output[0]
}

void main() {
    long sum = 0
    (0..<50).each { long y ->
        (0..<50).each { long x ->
            if (beam(x, y, "input.txt") == 1) {
                sum++
            }
        }
    }
    println sum
}

main()
