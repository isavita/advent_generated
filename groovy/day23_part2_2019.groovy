import java.util.ArrayDeque
import java.util.LinkedList

class IntcodeComputer {
    Map<Long, Long> mem = [:]
    long ip = 0
    long relativeBase = 0
    Deque<Long> inputs = new ArrayDeque<>()
    Deque<Long> outputs = new ArrayDeque<>()
    boolean halted = false
    boolean idle = false

    IntcodeComputer(List<Long> program) {
        for (int i = 0; i < program.size(); i++) {
            mem[(long) i] = program[i]
        }
        ip = 0
        relativeBase = 0
    }

    long memGet(long addr) {
        if (addr < 0) throw new RuntimeException("Negative memory access: ${addr}")
        mem.getOrDefault(addr, 0L)
    }

    void memSet(long addr, long val) {
        if (addr < 0) throw new RuntimeException("Negative memory write: ${addr}")
        mem[addr] = val
    }

    long getParamValue(int mode, int offset) {
        long addr = ip + offset
        long param = memGet(addr)
        switch (mode) {
            case 0: return memGet(param)
            case 1: return param
            case 2: return memGet(relativeBase + param)
            default: throw new RuntimeException("Unknown parameter mode: ${mode}")
        }
    }

    long getWriteAddr(int mode, int offset) {
        long addr = ip + offset
        long param = memGet(addr)
        switch (mode) {
            case 0: return param
            case 2: return relativeBase + param
            default: throw new RuntimeException("Invalid mode for write parameter: ${mode}")
        }
    }

    int runUntilPause() {
        idle = false
        while (true) {
            long instruction = memGet(ip)
            int opcode = (int) (instruction % 100)
            int m1 = (int) ((instruction / 100) % 10)
            int m2 = (int) ((instruction / 1000) % 10)
            int m3 = (int) ((instruction / 10000) % 10)

            switch (opcode) {
                case 1: {
                    long a = getParamValue(m1, 1)
                    long b = getParamValue(m2, 2)
                    long dest = getWriteAddr(m3, 3)
                    memSet(dest, a + b)
                    ip += 4
                    break
                }
                case 2: {
                    long a = getParamValue(m1, 1)
                    long b = getParamValue(m2, 2)
                    long dest = getWriteAddr(m3, 3)
                    memSet(dest, a * b)
                    ip += 4
                    break
                }
                case 3: {
                    long dest = getWriteAddr(m1, 1)
                    if (inputs.isEmpty()) {
                        idle = true
                        return -1
                    } else {
                        long v = inputs.removeFirst()
                        memSet(dest, v)
                        ip += 2
                    }
                    break
                }
                case 4: {
                    long v = getParamValue(m1, 1)
                    outputs.addLast(v)
                    ip += 2
                    if (outputs.size() >= 3) return 1
                    break
                }
                case 5: {
                    long p1 = getParamValue(m1, 1)
                    long p2 = getParamValue(m2, 2)
                    if (p1 != 0) ip = p2 else ip += 3
                    break
                }
                case 6: {
                    long p1 = getParamValue(m1, 1)
                    long p2 = getParamValue(m2, 2)
                    if (p1 == 0) ip = p2 else ip += 3
                    break
                }
                case 7: {
                    long a = getParamValue(m1, 1)
                    long b = getParamValue(m2, 2)
                    long dest = getWriteAddr(m3, 3)
                    memSet(dest, (a < b) ? 1 : 0)
                    ip += 4
                    break
                }
                case 8: {
                    long a = getParamValue(m1, 1)
                    long b = getParamValue(m2, 2)
                    long dest = getWriteAddr(m3, 3)
                    memSet(dest, (a == b) ? 1 : 0)
                    ip += 4
                    break
                }
                case 9: {
                    long a = getParamValue(m1, 1)
                    relativeBase += a
                    ip += 2
                    break
                }
                case 99:
                    halted = true
                    return -1
                default:
                    throw new RuntimeException("Unknown opcode ${opcode} at ip ${ip}")
            }
        }
    }
}

def programFile = new File("input.txt")
if (!programFile.exists()) {
    println "input.txt not found"
    System.exit(1)
}
def raw = programFile.text.trim()
if (raw.length() == 0) {
    println "0"
    System.exit(0)
}
def tokens = raw.split(',')
def program = tokens.collect { it.trim().toLong() }

final int NUM_COMPUTERS = 50
def computers = []
def packetQueues = []

for (i in 0..<NUM_COMPUTERS) {
    def c = new IntcodeComputer(program)
    c.inputs.addLast((long) i)
    computers << c
    packetQueues << new ArrayDeque<Long>()
}

Long natX = -1L
Long natY = -1L
Long prevNatY = Long.MIN_VALUE
boolean natHasPacket = false

while (true) {
    boolean networkIdle = true

    for (i in 0..<NUM_COMPUTERS) {
        def c = computers[i]
        if (c.halted) continue

        if (packetQueues[i].isEmpty()) {
            if (c.inputs.isEmpty()) {
                c.inputs.addLast(-1L)
            }
        } else {
            networkIdle = false
            long x = packetQueues[i].removeFirst()
            long y = packetQueues[i].removeFirst()
            c.inputs.addLast(x)
            c.inputs.addLast(y)
        }

        int status = c.runUntilPause()
        if (!c.idle) networkIdle = false

        while (c.outputs.size() >= 3) {
            long dest = c.outputs.removeFirst()
            long x = c.outputs.removeFirst()
            long y = c.outputs.removeFirst()
            if (dest == 255L) {
                natX = x
                natY = y
                natHasPacket = true
            } else if (dest >= 0 && dest < NUM_COMPUTERS) {
                packetQueues[(int) dest].addLast(x)
                packetQueues[(int) dest].addLast(y)
            }
        }
    }

    boolean allQueuesEmpty = true
    for (i in 0..<NUM_COMPUTERS) {
        if (!packetQueues[i].isEmpty()) {
            allQueuesEmpty = false
            break
        }
    }

    boolean allComputersIdle = true
    for (i in 0..<NUM_COMPUTERS) {
        def c = computers[i]
        if (!c.halted && !c.idle) {
            allComputersIdle = false
            break
        }
    }

    if (networkIdle && allQueuesEmpty && allComputersIdle) {
        if (natHasPacket) {
            if (natY == prevNatY) {
                println natY
                break
            }
            prevNatY = natY
            packetQueues[0].addLast(natX)
            packetQueues[0].addLast(natY)
            computers[0].idle = false
            natHasPacket = false
        } else {
            boolean allHalted = true
            for (i in 0..<NUM_COMPUTERS) {
                if (!computers[i].halted) { allHalted = false; break }
            }
            if (allHalted) {
                break
            }
        }
    }
}