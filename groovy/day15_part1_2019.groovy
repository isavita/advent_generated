import java.util.ArrayDeque
import java.util.HashSet
import java.util.HashMap

class IntcodeComputer {
    Map<Long, Long> memory = [:]
    long ip = 0L
    long relativeBase = 0L
    boolean halted = false
    Deque<Long> inputQueue = new ArrayDeque<>()
    Deque<Long> outputQueue = new ArrayDeque<>()

    void loadProgram(List<Long> program) {
        memory.clear()
        for (int i = 0; i < program.size(); i++) {
            memory[(long) i] = program[i]
        }
        ip = 0L
        relativeBase = 0L
        halted = false
        inputQueue.clear()
        outputQueue.clear()
    }

    private long read(long addr) {
        Long v = memory[addr]
        return v != null ? v : 0L
    }

    private void write(long addr, long val) {
        memory[addr] = val
    }

    private long getParam(int mode, int offset) {
        long param = read(ip + offset)
        if (mode == 0) return read(param)
        if (mode == 1) return param
        if (mode == 2) return read(relativeBase + param)
        throw new RuntimeException("Unknown parameter mode for reading: " + mode)
    }

    private void setParam(int mode, int offset, long value) {
        long target = read(ip + offset)
        if (mode == 0) write(target, value)
        else if (mode == 2) write(relativeBase + target, value)
        else throw new RuntimeException("Unknown parameter mode for writing: " + mode)
    }

    void run() {
        while (!halted) {
            long instruction = read(ip)
            int opcode = (int) (instruction % 100)
            int m1 = (int) ((instruction / 100) % 10)
            int m2 = (int) ((instruction / 1000) % 10)
            int m3 = (int) ((instruction / 10000) % 10)

            switch (opcode) {
                case 1:
                    long a = getParam(m1, 1)
                    long b = getParam(m2, 2)
                    setParam(m3, 3, a + b)
                    ip += 4
                    break
                case 2:
                    long c = getParam(m1, 1)
                    long d = getParam(m2, 2)
                    setParam(m3, 3, c * d)
                    ip += 4
                    break
                case 3:
                    if (inputQueue.isEmpty()) return
                    long inVal = inputQueue.removeFirst()
                    setParam(m1, 1, inVal)
                    ip += 2
                    break
                case 4:
                    long outVal = getParam(m1, 1)
                    outputQueue.add(outVal)
                    ip += 2
                    return
                case 5:
                    long t = getParam(m1, 1)
                    long j = getParam(m2, 2)
                    if (t != 0) ip = j else ip += 3
                    break
                case 6:
                    long f = getParam(m1, 1)
                    long g = getParam(m2, 2)
                    if (f == 0) ip = g else ip += 3
                    break
                case 7:
                    long p1 = getParam(m1, 1)
                    long p2 = getParam(m2, 2)
                    setParam(m3, 3, (p1 < p2) ? 1L : 0L)
                    ip += 4
                    break
                case 8:
                    long e1 = getParam(m1, 1)
                    long e2 = getParam(m2, 2)
                    setParam(m3, 3, (e1 == e2) ? 1L : 0L)
                    ip += 4
                    break
                case 9:
                    long adj = getParam(m1, 1)
                    relativeBase += adj
                    ip += 2
                    break
                case 99:
                    halted = true
                    return
                default:
                    throw new RuntimeException("Unknown opcode: " + opcode)
            }
        }
    }
}

class Droid {
    IntcodeComputer computer
    Map<Integer, int[]> directionMap
    List<Integer> currentPosition
    Map<List<Integer>, Integer> grid
    List<Integer> oxygenPosition
    boolean oxygenFound = false

    Droid(List<Long> program) {
        computer = new IntcodeComputer()
        computer.loadProgram(program)
        currentPosition = [0, 0]
        grid = [:]
        grid[currentPosition] = 1
        directionMap = [
            1: [0, -1],
            2: [0, 1],
            3: [-1, 0],
            4: [1, 0]
        ]
    }

    int sendMoveCommand(int direction) {
        computer.inputQueue.add((long) direction)
        while (computer.outputQueue.isEmpty() && !computer.halted) {
            computer.run()
        }
        if (computer.halted && computer.outputQueue.isEmpty()) {
            throw new RuntimeException("Intcode program halted unexpectedly before producing output.")
        }
        if (computer.outputQueue.isEmpty()) {
            throw new RuntimeException("Intcode program is waiting for input or in an unexpected state, but no output.")
        }
        long status = computer.outputQueue.removeFirst()
        return (int) status
    }

    int getOppositeDirection(int direction) {
        switch (direction) {
            case 1: return 2
            case 2: return 1
            case 3: return 4
            case 4: return 3
            default: throw new IllegalArgumentException("Invalid direction.")
        }
    }

    List<Integer> findPath(List<Integer> start, List<Integer> end) {
        def q = new ArrayDeque()
        q.add([start, []])
        def visited = new HashSet<List<Integer>>()
        visited.add(start)

        while (!q.isEmpty()) {
            def pair = q.removeFirst()
            def pos = pair[0]
            def path = pair[1]

            if (pos == end) {
                return path
            }

            for (def direction : [1, 2, 3, 4]) {
                def dx = directionMap[direction][0]
                def dy = directionMap[direction][1]
                def newPos = [pos[0] + dx, pos[1] + dy]

                if (visited.contains(newPos)) continue

                def itVal = grid[newPos]
                if (itVal == null || itVal == 0) continue

                visited.add(newPos)
                def newPath = path + [direction]
                q.add([newPos, newPath])
            }
        }
        throw new RuntimeException("No path found from start to end.")
    }

    void moveTo(List<Integer> target) {
        if (currentPosition == target) {
            return
        }
        def path = findPath(currentPosition, target)
        for (def direction : path) {
            int status = sendMoveCommand(direction)
            if (status == 0) {
                throw new RuntimeException("Unexpected wall encountered while moving to target position.")
            }
            def delta = directionMap[direction]
            currentPosition = [currentPosition[0] + delta[0], currentPosition[1] + delta[1]]
            if (status == 2) {
                oxygenPosition = currentPosition
                oxygenFound = true
            }
        }
    }

    int explore() {
        def q = new ArrayDeque()
        q.add([currentPosition, 0])
        def visitedExplore = new HashSet<List<Integer>>()
        visitedExplore.add(currentPosition)

        while (!q.isEmpty()) {
            def item = q.removeFirst()
            def pos = item[0]
            def steps = item[1]

            moveTo(pos)

            for (def dir : [1, 2, 3, 4]) {
                def delta = directionMap[dir]
                def newPos = [pos[0] + delta[0], pos[1] + delta[1]]
                if (visitedExplore.contains(newPos)) continue

                int status = sendMoveCommand(dir)
                if (status == 0) {
                    grid[newPos] = 0
                } else {
                    grid[newPos] = (status == 1) ? 1 : 2
                    visitedExplore.add(newPos)
                    q.add([newPos, steps + 1])
                    if (status == 2) {
                        oxygenPosition = newPos
                        oxygenFound = true
                        return steps + 1
                    }
                    int oppositeDirection = getOppositeDirection(dir)
                    sendMoveCommand(oppositeDirection)
                }
            }
        }
        return -1
    }
}

class Main {
    static List<Long> parseInput(String filePath) {
        def file = new File(filePath)
        if (!file.exists()) throw new RuntimeException("Could not open file: " + filePath)
        def text = file.text.trim()
        if (text.isEmpty()) return []
        def parts = text.split(',')
        def program = []
        for (p in parts) program << p.toLong()
        return program
    }

    static void main(String[] args) {
        def program = parseInput("input.txt")
        def droid = new Droid(program)
        int steps = droid.explore()
        println(steps)
    }
}

Main.main(null)