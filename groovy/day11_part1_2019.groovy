
class Intcode {
    List<Long> memory
    int ip = 0
    List<Long> input = []
    List<Long> output = []
    boolean halted = false

    Intcode(List<Long> program) {
        this.memory = new ArrayList<>(program)
    }

    void addInput(long input) {
        this.input.add(input)
    }

    void run() {
        output = []
        while (!halted) {
            long opcode = memory[ip] % 100
            switch (opcode) {
                case 1:
                case 2:
                case 7:
                case 8:
                    ensureMemory(ip + 3)
                    List<Integer> params = getParams(3)
                    long val1 = readMemory(params[0])
                    long val2 = readMemory(params[1])
                    long result
                    if (opcode == 1) {
                        result = val1 + val2
                    } else if (opcode == 2) {
                        result = val1 * val2
                    } else if (opcode == 7) {
                        result = val1 < val2 ? 1 : 0
                    } else {
                        result = val1 == val2 ? 1 : 0
                    }
                    writeMemory(params[2], result)
                    ip += 4
                    break
                case 3:
                case 4:
                    ensureMemory(ip + 1)
                    List<Integer> params = getParams(1)
                    if (opcode == 3) {
                        if (input.isEmpty()) {
                            return
                        }
                        writeMemory(params[0], input.remove(0))
                    } else {
                        output.add(readMemory(params[0]))
                    }
                    ip += 2
                    break
                case 5:
                case 6:
                    ensureMemory(ip + 2)
                    List<Integer> params = getParams(2)
                    long val = readMemory(params[0])
                    long target = readMemory(params[1])
                    if ((opcode == 5 && val != 0) || (opcode == 6 && val == 0)) {
                        ip = (int) target
                    } else {
                        ip += 3
                    }
                    break
                case 99:
                    halted = true
                    return
                default:
                    throw new Exception("Unknown opcode: " + opcode)
            }
        }
    }

    long readMemory(int address) {
        ensureMemory(address)
        return memory[address]
    }

    void writeMemory(int address, long value) {
        ensureMemory(address)
        memory[address] = value
    }

    void ensureMemory(int address) {
        while (address >= memory.size()) {
            memory.add(0L)
        }
    }

    List<Integer> getParams(int count) {
        long paramModes = memory[ip] / 100
        List<Integer> params = []
        for (int i = 0; i < count; i++) {
            if (paramModes % 10 == 1) {
                params.add(ip + i + 1)
            } else {
                params.add((int) memory[ip + i + 1])
            }
            paramModes /= 10
        }
        return params
    }

    List<Long> getOutputs() {
        return output
    }

    boolean isHalted() {
        return halted
    }
}

class Robot {
    Point position = new Point(0, 0)
    int direction = 0  // 0: Up, 1: Right, 2: Down, 3: Left

    void turnAndMove(long turnDirection) {
        if (turnDirection == 0) {
            direction = (direction - 1 + 4) % 4  // Turn left
        } else {
            direction = (direction + 1) % 4  // Turn right
        }

        switch (direction) {
            case 0:
                position = new Point(position.x, position.y - 1)
                break
            case 1:
                position = new Point(position.x + 1, position.y)
                break
            case 2:
                position = new Point(position.x, position.y + 1)
                break
            case 3:
                position = new Point(position.x - 1, position.y)
                break
        }
    }
}

class Point {
    int x, y
    Point(int x, int y) {
        this.x = x
        this.y = y
    }

    @Override
    int hashCode() {
        return Objects.hash(x, y)
    }

    @Override
    boolean equals(Object obj) {
        if (this.is(obj)) return true
        if (getClass() != obj.class) return false
        Point other = (Point) obj
        return x == other.x && y == other.y
    }
}

void main() {
    File file = new File("input.txt")
    List<Long> program = file.text.trim().split(',').collect { it.toLong() }

    Map<Point, Long> grid = [:]
    Robot robot = new Robot()
    Intcode intcode = new Intcode(program)

    while (!intcode.isHalted()) {
        long currentColor = grid.getOrDefault(robot.position, 0L)
        intcode.addInput(currentColor)
        intcode.run()
        List<Long> outputs = intcode.getOutputs()

        if (outputs.size() == 2) {
            grid[robot.position] = outputs[0]
            robot.turnAndMove(outputs[1])
        }
    }

    println grid.size()
}

main()
