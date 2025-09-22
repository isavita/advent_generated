import java.util.ArrayDeque
import java.util.Arrays

class Main {
    static final int MAX_GRID_DIM = 100
    static final int MAX_MEM_SIZE = 10000
    static final int MAX_FUNC_LEN = 21

    static class IntcodeComputer {
        List<Long> memory
        int pointer
        long relativeBase
        ArrayDeque<Long> inputs = new ArrayDeque<>()
        ArrayDeque<Long> outputs = new ArrayDeque<>()
        boolean halted = false
        boolean waitingForInput = false

        IntcodeComputer(List<Long> program) {
            memory = new ArrayList<>(program)
            pointer = 0
            relativeBase = 0
        }

        private void ensure(int addr) {
            while (addr >= memory.size()) memory.add(0L)
        }

        private long getMem(long addr) {
            ensure((int)addr)
            return memory[(int)addr]
        }

        private void setMem(long addr, long value) {
            ensure((int)addr)
            memory[(int)addr] = value
        }

        private long getParam(int mode, long paramAddr) {
            long raw = getMem(paramAddr)
            switch (mode) {
                case 0: return getMem(raw)
                case 1: return raw
                case 2: return getMem(relativeBase + raw)
                default: throw new RuntimeException("Unknown param mode: $mode")
            }
        }

        private void setParam(int mode, long paramAddr, long value) {
            long raw = getMem(paramAddr)
            switch (mode) {
                case 0: setMem(raw, value); break
                case 2: setMem(relativeBase + raw, value); break
                default: throw new RuntimeException("Unknown write mode: $mode")
            }
        }

        void run() {
            waitingForInput = false
            while (!halted && !waitingForInput) {
                long instruction = getMem((long)pointer)
                int opcode = (int)(instruction % 100)
                int m1 = (int)((instruction / 100) % 10)
                int m2 = (int)((instruction / 1000) % 10)
                int m3 = (int)((instruction / 10000) % 10)

                switch (opcode) {
                    case 1:
                        long a = getParam(m1, pointer + 1)
                        long b = getParam(m2, pointer + 2)
                        setParam(m3, pointer + 3, a + b)
                        pointer += 4
                        break
                    case 2:
                        long c = getParam(m1, pointer + 1)
                        long d = getParam(m2, pointer + 2)
                        setParam(m3, pointer + 3, c * d)
                        pointer += 4
                        break
                    case 3:
                        if (inputs.isEmpty()) {
                            waitingForInput = true
                            return
                        }
                        long inVal = inputs.removeFirst()
                        setParam(m1, pointer + 1, inVal)
                        pointer += 2
                        break
                    case 4:
                        long outVal = getParam(m1, pointer + 1)
                        outputs.addLast(outVal)
                        pointer += 2
                        break
                    case 5:
                        long p1 = getParam(m1, pointer + 1)
                        long p2 = getParam(m2, pointer + 2)
                        if (p1 != 0) pointer = (int)p2 else pointer += 3
                        break
                    case 6:
                        long q1 = getParam(m1, pointer + 1)
                        long q2 = getParam(m2, pointer + 2)
                        if (q1 == 0) pointer = (int)q2 else pointer += 3
                        break
                    case 7:
                        long r1 = getParam(m1, pointer + 1)
                        long r2 = getParam(m2, pointer + 2)
                        long val = (r1 < r2) ? 1L : 0L
                        setParam(m3, pointer + 3, val)
                        pointer += 4
                        break
                    case 8:
                        long s1 = getParam(m1, pointer + 1)
                        long s2 = getParam(m2, pointer + 2)
                        long eq = (s1 == s2) ? 1L : 0L
                        setParam(m3, pointer + 3, eq)
                        pointer += 4
                        break
                    case 9:
                        long t = getParam(m1, pointer + 1)
                        relativeBase += t
                        pointer += 2
                        break
                    case 99:
                        halted = true
                        break
                    default:
                        throw new RuntimeException("Unknown opcode: $opcode at pointer $pointer")
                }
            }
        }
    }

    static class RobotState {
        int x
        int y
        char dir
        RobotState(int x, int y, char d) { this.x = x; this.y = y; this.dir = d }
    }

    static char turnLeft(char dir) {
        switch (dir) {
            case '^': return '<'
            case '<': return 'v'
            case 'v': return '>'
            case '>': return '^'
            default: return '?'
        }
    }

    static char turnRight(char dir) {
        switch (dir) {
            case '^': return '>'
            case '>': return 'v'
            case 'v': return '<'
            case '<': return '^'
            default: return '?'
        }
    }

    static void main(String[] args) {
        File infile = new File("input.txt")
        if (!infile.exists()) {
            System.err.println("input.txt not found")
            return
        }

        String content = infile.text
        List<Long> program = content.split(',').collect { it.trim() }.findAll { it.length() > 0 }.collect { Long.parseLong(it) }

        // Part One
        IntcodeComputer comp1 = new IntcodeComputer(program)
        comp1.run()
        List<Long> outList = []
        while (!comp1.outputs.isEmpty()) {
            outList.add(comp1.outputs.removeFirst())
        }

        int width = 0
        int height = 0
        char[][] grid = new char[MAX_GRID_DIM][MAX_GRID_DIM]
        for (int i = 0; i < MAX_GRID_DIM; i++) Arrays.fill(grid[i], (char)0)

        int x = 0
        int y = 0
        int gridWidth = 0
        int gridHeight = 0
        for (Long v : outList) {
            if (v == 10L) {
                if (x > 0) {
                    if (gridWidth == 0) gridWidth = x
                    y++
                    x = 0
                    if (y >= MAX_GRID_DIM) {
                        throw new RuntimeException("Grid height exceeds limit")
                    }
                }
            } else {
                if (x >= MAX_GRID_DIM) throw new RuntimeException("Grid width exceeds limit")
                grid[y][x] = (char)(v.longValue())
                x++
            }
        }
        gridHeight = y
        if (x > 0 && gridWidth == 0) gridWidth = x
        if (x > 0 && y == gridHeight) gridHeight++

        long alignmentSum = 0
        for (int r = 1; r < gridHeight - 1; r++) {
            for (int c = 1; c < gridWidth - 1; c++) {
                if (grid[r][c] == '#' &&
                    grid[r - 1][c] == '#' && grid[r + 1][c] == '#' &&
                    grid[r][c - 1] == '#' && grid[r][c + 1] == '#') {
                    alignmentSum += (long)r * c
                }
            }
        }
        println "Part One: ${alignmentSum}"

        // Part Two
        List<Long> program2 = new ArrayList<>(program)
        program2.set(0, 2L)
        IntcodeComputer comp2 = new IntcodeComputer(program2)

        // Find robot
        RobotState robot = null
        for (int r = 0; r < gridHeight; r++) {
            for (int c = 0; c < gridWidth; c++) {
                char ch = grid[r][c]
                if (ch == '^' || ch == 'v' || ch == '<' || ch == '>') {
                    robot = new RobotState(c, r, ch)
                    break
                }
            }
            if (robot != null) break
        }
        if (robot == null) {
            System.err.println("Robot not found")
            return
        }

        // Build movement path
        List<String> pathTokens = []
        int gx = robot.x
        int gy = robot.y
        char gdir = robot.dir
        int steps = 0

        while (true) {
            int nx = gx, ny = gy
            switch (gdir) {
                case '^': ny = gy - 1; break
                case 'v': ny = gy + 1; break
                case '<': nx = gx - 1; break
                case '>': nx = gx + 1; break
            }
            boolean canForward = ny >= 0 && ny < gridHeight && nx >= 0 && nx < gridWidth && grid[ny][nx] == '#'
            if (canForward) {
                gx = nx
                gy = ny
                steps++
                continue
            } else {
                if (steps > 0) { pathTokens << steps.toString(); steps = 0 }
                char leftDir = turnLeft(gdir)
                int lx = gx, ly = gy
                switch (leftDir) {
                    case '^': ly = gy - 1; break
                    case 'v': ly = gy + 1; break
                    case '<': lx = gx - 1; break
                    case '>': lx = gx + 1; break
                }
                boolean canLeft = ly >= 0 && ly < gridHeight && lx >= 0 && lx < gridWidth && grid[ly][lx] == '#'
                if (canLeft) {
                    pathTokens << 'L'
                    gdir = leftDir
                    continue
                }
                char rightDir = turnRight(gdir)
                int rx = gx, ry = gy
                switch (rightDir) {
                    case '^': ry = gy - 1; break
                    case 'v': ry = gy + 1; break
                    case '<': rx = gx - 1; break
                    case '>': rx = gx + 1; break
                }
                boolean canRight = ry >= 0 && ry < gridHeight && rx >= 0 && rx < gridWidth && grid[ry][rx] == '#'
                if (canRight) {
                    pathTokens << 'R'
                    gdir = rightDir
                    continue
                }
                break
            }
        }

        // Helper to join tokens
        def joinTokens = { List<String> tokens, int start, int len ->
            def sb = new StringBuilder()
            for (int i = 0; i < len; i++) {
                if (i > 0) sb.append(',')
                sb.append(tokens[start + i])
            }
            sb.toString()
        }

        def tokensMatchSegment = { List<String> tokens, int start, List<String> base, int baseStart, int len ->
            for (int i = 0; i < len; i++) {
                if (start + i >= tokens.size()) return false
                if (tokens[start + i] != base[baseStart + i]) return false
            }
            return true
        }

        def pathFound = false
        def mainRoutine = ""
        def funcA = ""
        def funcB = ""
        def funcC = ""

        def pathSize = pathTokens.size()
        def maxLen = MAX_FUNC_LEN

        // Brute-force compression
        for (int aLen = 1; aLen <= 10 && aLen <= pathSize && !pathFound; aLen++) {
            def A = joinTokens(pathTokens, 0, aLen)
            if (A.length() > maxLen) continue
            int bStart = 0
            while (bStart < pathSize && tokensMatchSegment(pathTokens, bStart, pathTokens, 0, aLen)) {
                bStart += aLen
            }
            if (bStart >= pathSize) continue
            for (int bLen = 1; bLen <= 10 && bStart + bLen <= pathSize && !pathFound; bLen++) {
                def B = joinTokens(pathTokens, bStart, bLen)
                if (B.length() > maxLen) continue
                int cStart = 0
                while (cStart < pathSize) {
                    if (tokensMatchSegment(pathTokens, cStart, pathTokens, 0, aLen)) {
                        cStart += aLen
                    } else if (tokensMatchSegment(pathTokens, cStart, pathTokens, bStart, bLen)) {
                        cStart += bLen
                    } else {
                        break
                    }
                }
                if (cStart >= pathSize) continue
                for (int cLen = 1; cLen <= 10 && cStart + cLen <= pathSize && !pathFound; cLen++) {
                    def C = joinTokens(pathTokens, cStart, cLen)
                    if (C.length() > maxLen) continue

                    // Try build main routine
                    def mainParts = []
                    int pos = 0
                    boolean ok = true
                    while (pos < pathSize) {
                        boolean matched = false
                        if (tokensMatchSegment(pathTokens, pos, pathTokens, 0, aLen)) {
                            mainParts << 'A'
                            pos += aLen
                            matched = true
                        } else if (tokensMatchSegment(pathTokens, pos, pathTokens, bStart, bLen)) {
                            mainParts << 'B'
                            pos += bLen
                            matched = true
                        } else if (tokensMatchSegment(pathTokens, pos, pathTokens, cStart, cLen)) {
                            mainParts << 'C'
                            pos += cLen
                            matched = true
                        }
                        if (!matched) { ok = false; break }
                        def currentMain = mainParts.join(',')
                        if (currentMain.length() >= maxLen) { ok = false; break }
                    }
                    if (ok && pos == pathSize) {
                        mainRoutine = mainParts.join(',')
                        funcA = A
                        funcB = B
                        funcC = C
                        pathFound = true
                    }
                }
            }
        }

        if (!pathFound) {
            System.err.println("Failed to compress path")
            return
        }

        // Prepare input for Part Two
        def inputs = [mainRoutine, funcA, funcB, funcC, "n"]
        inputs.each { str ->
            str.each { ch ->
                comp2.inputs.addLast((long) ch as Long)
            }
            comp2.inputs.addLast(10L) // newline
        }

        comp2.run()
        if (comp2.waitingForInput && !comp2.inputs.isEmpty()) {
            comp2.run()
        }

        long dustCollected = 0
        while (!comp2.outputs.isEmpty()) {
            dustCollected = comp2.outputs.removeFirst()
        }

        println "Part Two: ${dustCollected}"
    }
}