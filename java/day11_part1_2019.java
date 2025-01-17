
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class SpacePolice {

    public static void main(String[] args) {
        try {
            String input = readFile("input.txt");
            long[] program = Arrays.stream(input.split(",")).mapToLong(Long::parseLong).toArray();

            int paintedPanels = paintHull(program);
            System.out.println(paintedPanels);

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static int paintHull(long[] program) {
        Map<Point, Integer> hull = new HashMap<>();
        Point robotPos = new Point(0, 0);
        int robotDir = 0; // 0: up, 1: right, 2: down, 3: left

        IntcodeComputer computer = new IntcodeComputer(program);
        while (!computer.isHalted()) {
            int panelColor = hull.getOrDefault(robotPos, 0);
            computer.addInput(panelColor);

            Long paintColor = computer.runUntilOutput();
            Long turnDir = computer.runUntilOutput();

            if (paintColor == null || turnDir == null) break;

            hull.put(robotPos, paintColor.intValue());

            robotDir = (robotDir + (turnDir == 0 ? -1 : 1) + 4) % 4;

            switch (robotDir) {
                case 0:
                    robotPos = new Point(robotPos.x, robotPos.y + 1);
                    break;
                case 1:
                    robotPos = new Point(robotPos.x + 1, robotPos.y);
                    break;
                case 2:
                    robotPos = new Point(robotPos.x, robotPos.y - 1);
                    break;
                case 3:
                    robotPos = new Point(robotPos.x - 1, robotPos.y);
                    break;
            }
        }

        return hull.size();
    }

    private static String readFile(String filename) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(filename));
        String line = reader.readLine();
        reader.close();
        return line;
    }

    static class Point {
        int x, y;

        public Point(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Point point = (Point) o;
            return x == point.x && y == point.y;
        }

        @Override
        public int hashCode() {
            return Objects.hash(x, y);
        }
    }

    static class IntcodeComputer {
        private long[] memory;
        private int ip;
        private long relativeBase;
        private Queue<Long> inputQueue;
        private Queue<Long> outputQueue;

        public IntcodeComputer(long[] program) {
            this.memory = Arrays.copyOf(program, program.length * 10); // Expand memory
            this.ip = 0;
            this.relativeBase = 0;
            this.inputQueue = new LinkedList<>();
            this.outputQueue = new LinkedList<>();
        }

        public void addInput(long input) {
            inputQueue.add(input);
        }

        public Long runUntilOutput() {
            while (true) {
                int opcode = (int) (memory[ip] % 100);
                int[] modes = getParameterModes(memory[ip]);

                if (opcode == 99) {
                    return null; // Halt
                }

                if (opcode == 1) { // Add
                    long val1 = getParameterValue(1, modes[0]);
                    long val2 = getParameterValue(2, modes[1]);
                    setParameterValue(3, modes[2], val1 + val2);
                    ip += 4;
                } else if (opcode == 2) { // Multiply
                    long val1 = getParameterValue(1, modes[0]);
                    long val2 = getParameterValue(2, modes[1]);
                    setParameterValue(3, modes[2], val1 * val2);
                    ip += 4;
                } else if (opcode == 3) { // Input
                    if (inputQueue.isEmpty()) {
                        return null; // Wait for input
                    }
                    setParameterValue(1, modes[0], inputQueue.poll());
                    ip += 2;
                } else if (opcode == 4) { // Output
                    long output = getParameterValue(1, modes[0]);
                    outputQueue.add(output);
                    ip += 2;
                    return output;
                } else if (opcode == 5) { // Jump-if-true
                    long val1 = getParameterValue(1, modes[0]);
                    long val2 = getParameterValue(2, modes[1]);
                    ip = (val1 != 0) ? (int) val2 : ip + 3;
                } else if (opcode == 6) { // Jump-if-false
                    long val1 = getParameterValue(1, modes[0]);
                    long val2 = getParameterValue(2, modes[1]);
                    ip = (val1 == 0) ? (int) val2 : ip + 3;
                } else if (opcode == 7) { // Less than
                    long val1 = getParameterValue(1, modes[0]);
                    long val2 = getParameterValue(2, modes[1]);
                    setParameterValue(3, modes[2], (val1 < val2) ? 1 : 0);
                    ip += 4;
                } else if (opcode == 8) { // Equals
                    long val1 = getParameterValue(1, modes[0]);
                    long val2 = getParameterValue(2, modes[1]);
                    setParameterValue(3, modes[2], (val1 == val2) ? 1 : 0);
                    ip += 4;
                } else if (opcode == 9) { // Adjust relative base
                    long val1 = getParameterValue(1, modes[0]);
                    relativeBase += val1;
                    ip += 2;
                } else {
                    throw new RuntimeException("Invalid opcode: " + opcode);
                }
            }
        }

        public boolean isHalted() {
            return memory[ip] == 99;
        }

        private int[] getParameterModes(long instruction) {
            int[] modes = new int[3];
            modes[0] = (int) ((instruction / 100) % 10);
            modes[1] = (int) ((instruction / 1000) % 10);
            modes[2] = (int) ((instruction / 10000) % 10);
            return modes;
        }

        private long getParameterValue(int paramNum, int mode) {
            long param = memory[ip + paramNum];
            if (mode == 0) { // Position mode
                return memory[(int) param];
            } else if (mode == 1) { // Immediate mode
                return param;
            } else if (mode == 2) { // Relative mode
                return memory[(int) (relativeBase + param)];
            } else {
                throw new RuntimeException("Invalid parameter mode: " + mode);
            }
        }

        private void setParameterValue(int paramNum, int mode, long value) {
            long param = memory[ip + paramNum];
            if (mode == 0) { // Position mode
                memory[(int) param] = value;
            } else if (mode == 2) { // Relative mode
                memory[(int) (relativeBase + param)] = value;
            } else {
                throw new RuntimeException("Invalid parameter mode for writing: " + mode);
            }
        }
    }
}
