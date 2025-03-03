
class Duet {

    static long solvePart1(List<String> instructions) {
        Map<String, Long> registers = [:]
        long lastSound = 0
        long recoveredFrequency = 0

        for (int i = 0; i < instructions.size();) {
            String[] parts = instructions[i].split(" ")
            String instruction = parts[0]
            String register = parts[1]

            def getValue = { regOrVal ->
                regOrVal.isLong() ? regOrVal.toLong() : registers.getOrDefault(regOrVal, 0L)
            }

            long xValue = getValue(register)
            long yValue = parts.size() > 2 ? getValue(parts[2]) : 0

            switch (instruction) {
                case "snd":
                    lastSound = xValue
                    i++
                    break
                case "set":
                    registers.put(register, yValue)
                    i++
                    break
                case "add":
                    registers.put(register, xValue + yValue)
                    i++
                    break
                case "mul":
                    registers.put(register, xValue * yValue)
                    i++
                    break
                case "mod":
                    registers.put(register, xValue % yValue)
                    i++
                    break
                case "rcv":
                    if (xValue != 0) {
                        recoveredFrequency = lastSound
                        return recoveredFrequency // Stop on first successful rcv
                    }
                    i++
                    break
                case "jgz":
                    if (xValue > 0) {
                        i += yValue
                    } else {
                        i++
                    }
                    break
                default:
                    i++ // Handle unexpected instructions gracefully
            }
        }
        return recoveredFrequency
    }


    static long solvePart2(List<String> instructions) {
        Program program0 = new Program(0, instructions)
        Program program1 = new Program(1, instructions)

        program0.other = program1
        program1.other = program0

        while (!program0.waiting || !program1.waiting) {  //Simplified termination condition
            boolean p0Ran = program0.run()
            boolean p1Ran = program1.run()
             if (!p0Ran && !p1Ran) {
                break; // Deadlock: Both programs made no progress
            }

        }
        return program1.sendCount
    }

    static class Program {
        long id
        List<String> instructions
        Map<String, Long> registers = [:]
        int instructionPointer = 0
        Program other
        Queue<Long> receiveQueue = new LinkedList<>()
        long sendCount = 0
        boolean waiting = false

        Program(long id, List<String> instructions) {
            this.id = id
            this.instructions = instructions
            registers.put("p", id)
        }

        boolean run() {
            if (instructionPointer < 0 || instructionPointer >= instructions.size()) {
                waiting = true; // Treat out-of-bounds as waiting/termination
                return false;
            }

            String[] parts = instructions[instructionPointer].split(" ")
            String instruction = parts[0]
            String register = parts[1]

            def getValue = { regOrVal ->
                regOrVal.isLong() ? regOrVal.toLong() : registers.getOrDefault(regOrVal, 0L)
            }

            long xValue = getValue(register)
            long yValue = parts.size() > 2 ? getValue(parts[2]) : 0
            boolean progressed = true; //Assume instruction will execute
            waiting = false;

            switch (instruction) {
                case "snd":
                    other.receiveQueue.offer(xValue)
                    sendCount++
                    instructionPointer++
                    break
                case "set":
                    registers.put(register, yValue)
                    instructionPointer++
                    break
                case "add":
                    registers.put(register, xValue + yValue)
                    instructionPointer++
                    break
                case "mul":
                    registers.put(register, xValue * yValue)
                    instructionPointer++
                    break
                case "mod":
                    registers.put(register, xValue % yValue)
                    instructionPointer++
                    break
                case "rcv":
                    if (!receiveQueue.isEmpty()) {
                        registers.put(register, receiveQueue.poll())
                        instructionPointer++
                    } else {
                        waiting = true // Explicitly set waiting status
                        progressed = false //Indicate no instruction executed
                    }
                    break
                case "jgz":
                    if (xValue > 0) {
                        instructionPointer += yValue
                    } else {
                        instructionPointer++
                    }
                    break
                default:
                    instructionPointer++
            }
           return progressed;
        }
    }

    static void main(String[] args) {
        File inputFile = new File("input.txt")
        List<String> instructions = inputFile.readLines()

        println "Part 1: " + solvePart1(instructions)
        println "Part 2: " + solvePart2(instructions)
    }
}
