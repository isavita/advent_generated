
class IntcodeComputer {
    List<Long> program
    int ip = 0
    int relativeBase = 0
    List<Long> input = []
    List<Long> output = []
    boolean halt = false

    IntcodeComputer(List<Long> program) {
        this.program = program.collect { it }
        // Pad with extra memory
        (0..10000).each { this.program.add(0L) }
    }
    
    long getParam(int mode, int offset) {
        switch (mode) {
            case 0: return program[(program[ip + offset]).intValue()] // Position mode
            case 1: return program[ip + offset] // Immediate mode
            case 2: return program[(program[ip + offset] + relativeBase).intValue()] // Relative mode
            default: throw new IllegalArgumentException("Invalid parameter mode: $mode")
        }
    }

    void setParam(int mode, int offset, long value) {
        switch (mode) {
            case 0: program[(program[ip + offset]).intValue()] = value; break
            case 2: program[(program[ip + offset] + relativeBase).intValue()] = value; break
            default: throw new IllegalArgumentException("Invalid parameter mode for set: $mode")
        }
    }

    void run() {
        while (!halt) {
            int opcode = (program[ip] % 100).intValue()
            int mode1 = (program[ip] / 100 % 10).intValue()
            int mode2 = (program[ip] / 1000 % 10).intValue()
            int mode3 = (program[ip] / 10000 % 10).intValue()

            switch (opcode) {
                case 1: // Add
                    setParam(mode3, 3, getParam(mode1, 1) + getParam(mode2, 2))
                    ip += 4
                    break
                case 2: // Multiply
                    setParam(mode3, 3, getParam(mode1, 1) * getParam(mode2, 2))
                    ip += 4
                    break
                case 3: // Input
                    if (input.isEmpty()) {
                        return // Pause execution if no input
                    }
                    setParam(mode1, 1, input.remove(0))
                    ip += 2
                    break
                case 4: // Output
                    output.add(getParam(mode1, 1))
                    ip += 2
                    break
                case 5: // Jump-if-true
                    if (getParam(mode1, 1) != 0) {
                        ip = getParam(mode2, 2).intValue()
                    } else {
                        ip += 3
                    }
                    break
                case 6: // Jump-if-false
                    if (getParam(mode1, 1) == 0) {
                        ip = getParam(mode2, 2).intValue()
                    } else {
                        ip += 3
                    }
                    break
                case 7: // Less than
                    setParam(mode3, 3, getParam(mode1, 1) < getParam(mode2, 2) ? 1 : 0)
                    ip += 4
                    break
                case 8: // Equals
                    setParam(mode3, 3, getParam(mode1, 1) == getParam(mode2, 2) ? 1 : 0)
                    ip += 4
                    break
                case 9: // Adjust relative base
                    relativeBase += getParam(mode1, 1).intValue()
                    ip += 2
                    break
                case 99: // Halt
                    halt = true
                    break
                default:
                    throw new IllegalArgumentException("Invalid opcode: $opcode")
            }
        }
    }
}


class RepairDroid {
    IntcodeComputer computer
    Map<List<Integer>, Integer> grid = [:]
    int shortestPath = -1
    int fillTime = -1

    RepairDroid(List<Long> program) {
        this.computer = new IntcodeComputer(program)
    }

    void explore(int x, int y, int steps) {
        if (grid.containsKey([x, y]) && grid[[x, y]] <= steps && grid[[x,y]] != -2 ) {
            return // Already visited with fewer or equal steps, and it's not the goal.
        }

        grid[[x, y]] = steps

        for (int dir = 1; dir <= 4; dir++) {
            computer.input.add((long)dir)
            computer.run()
            int status = computer.output.remove(0).intValue()

            int nx = x, ny = y
            switch (dir) {
                case 1: ny--; break // North
                case 2: ny++; break // South
                case 3: nx--; break // West
                case 4: nx++; break // East
            }

            if (status != 0) { // Not a wall
               if (status == 2) {
                    grid[[nx, ny]] = -2;
                    if (shortestPath == -1 || steps + 1 < shortestPath)
                    {
                        shortestPath = steps+1
                    }
                    
                } else
               {
                    explore(nx, ny, steps + 1)
               }
                // Backtrack
                int backDir = (dir % 2 == 0) ? dir - 1 : dir + 1
                computer.input.add((long)backDir)
                computer.run()
                computer.output.remove(0) // Consume backtrack output
            }
        }
    }
    
    void calculateFillTime() {
        
        List<List<Integer>> oxygenLocations = []
        grid.each { pos, value ->
            if (value == -2) {
                oxygenLocations.add(pos)
            }
        }
        
        int time = 0
        
        while (true)
        {
            List<List<Integer>> newOxygenLocations = []
            oxygenLocations.each { oxPos ->
             for (int dir = 1; dir <= 4; dir++) {
                    int nx = oxPos[0]
                    int ny = oxPos[1]
                    switch (dir) {
                        case 1: ny--; break // North
                        case 2: ny++; break // South
                        case 3: nx--; break // West
                        case 4: nx++; break // East
                    }    
                   
                    if (grid.containsKey([nx,ny]) && grid[[nx,ny]] != -2 && grid[[nx,ny]] != -1 ) {
                       newOxygenLocations.add([nx,ny])
                    }
                }
            }
            
            if (newOxygenLocations.isEmpty())
            {
               break; 
            }
            
            boolean hasChanges = false;
            newOxygenLocations.each { pos ->
                if(grid[pos] != -2)
                {
                    grid[pos] = -2
                    oxygenLocations.add(pos)
                    hasChanges = true;
                }
            }
            if (!hasChanges)
            {
                break;
            }

            time++;
        }
         fillTime = time
        
    }
}


static void main(String[] args) {
    def inputFile = new File("input.txt")
    def program = inputFile.text.trim().split(',').collect { it.toLong() }

    def droid = new RepairDroid(program)
    droid.explore(0, 0, 0) // Start at (0, 0) with 0 steps

     println("Part 1: Shortest path to oxygen system: ${droid.shortestPath}")
    droid.calculateFillTime()
    println("Part 2: Time to fill with oxygen: ${droid.fillTime}")
}
