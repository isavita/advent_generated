
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class Day11 {

    public static void main(String[] args) throws IOException, InterruptedException {
        String input = readFile("input.txt");
        long[] program = Arrays.stream(input.split(",")).mapToLong(Long::parseLong).toArray();

        // Part 1
        int paintedPanelsPart1 = runRobot(program, 0).size();
        System.out.println("Part 1: Number of panels painted at least once: " + paintedPanelsPart1);

        // Part 2
        Map<Point, Integer> hull = runRobot(program, 1);
        printRegistrationIdentifier(hull);
    }

    private static Map<Point, Integer> runRobot(long[] program, int initialPanelColor) throws InterruptedException {
        BlockingQueue<Long> inputQueue = new LinkedBlockingQueue<>();
        BlockingQueue<Long> outputQueue = new LinkedBlockingQueue<>();
        IntcodeComputer computer = new IntcodeComputer(program.clone(), inputQueue, outputQueue);
        Thread computerThread = new Thread(computer);
        computerThread.start();

        Map<Point, Integer> hull = new HashMap<>();
        Point currentPosition = new Point(0, 0);
        int currentDirection = 0; // 0: up, 1: right, 2: down, 3: left

        hull.put(currentPosition, initialPanelColor); // Initialize the starting panel
        inputQueue.put((long) initialPanelColor);

        while (computerThread.isAlive() || !outputQueue.isEmpty()) {
           
            if(outputQueue.size() < 2 && computerThread.isAlive()) {
               continue; // Intcode has more work to do
            }
            if (outputQueue.isEmpty() && !computerThread.isAlive())
            {
                break; // Intcode finished
            }

            int paintColor = outputQueue.take().intValue();
            int turnDirection = outputQueue.take().intValue();

            hull.put(currentPosition, paintColor);

            // Update direction
            if (turnDirection == 0) { // Turn left
                currentDirection = (currentDirection + 3) % 4;
            } else { // Turn right
                currentDirection = (currentDirection + 1) % 4;
            }

            // Move forward
            switch (currentDirection) {
                case 0: // Up
                    currentPosition = new Point(currentPosition.x, currentPosition.y - 1);
                    break;
                case 1: // Right
                    currentPosition = new Point(currentPosition.x + 1, currentPosition.y);
                    break;
                case 2: // Down
                    currentPosition = new Point(currentPosition.x, currentPosition.y + 1);
                    break;
                case 3: // Left
                    currentPosition = new Point(currentPosition.x - 1, currentPosition.y);
                    break;
            }

            // Provide input for the next iteration
            int currentPanelColor = hull.getOrDefault(currentPosition, 0); // Default to black
             if(computerThread.isAlive()) {
                  inputQueue.put((long) currentPanelColor);
             }

        }

        return hull;
    }


    private static void printRegistrationIdentifier(Map<Point, Integer> hull) {
        int minX = Integer.MAX_VALUE, maxX = Integer.MIN_VALUE;
        int minY = Integer.MAX_VALUE, maxY = Integer.MIN_VALUE;

        for (Point p : hull.keySet()) {
            minX = Math.min(minX, p.x);
            maxX = Math.max(maxX, p.x);
            minY = Math.min(minY, p.y);
            maxY = Math.max(maxY, p.y);
        }

        System.out.println("Part 2: Registration Identifier:");
        for (int y = minY; y <= maxY; y++) {
            for (int x = minX; x <= maxX; x++) {
                int color = hull.getOrDefault(new Point(x, y), 0);
                System.out.print(color == 1 ? "#" : " ");
            }
            System.out.println();
        }
    }

    private static String readFile(String filename) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            return br.readLine();
        }
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

    static class IntcodeComputer implements Runnable {
        private final long[] memory;
        private long instructionPointer;
        private long relativeBase;
        private final BlockingQueue<Long> inputQueue;
        private final BlockingQueue<Long> outputQueue;

        public IntcodeComputer(long[] program, BlockingQueue<Long> inputQueue, BlockingQueue<Long> outputQueue) {
            this.memory = Arrays.copyOf(program, program.length * 10); // Increase memory
            this.instructionPointer = 0;
            this.relativeBase = 0;
            this.inputQueue = inputQueue;
            this.outputQueue = outputQueue;
        }


        @Override
        public void run() {
            while (true) {
                long opcode = memory[(int) instructionPointer] % 100;
                switch ((int) opcode) {
                    case 1: // Add
                        write(3, read(1) + read(2));
                        instructionPointer += 4;
                        break;
                    case 2: // Multiply
                        write(3, read(1) * read(2));
                        instructionPointer += 4;
                        break;
                    case 3: // Input
                        try {
                            write(1, inputQueue.take());
                        } catch (InterruptedException e) {
                          Thread.currentThread().interrupt();
                          return; // Exit if interrupted
                        }

                        instructionPointer += 2;
                        break;
                    case 4: // Output
                        try {
                            outputQueue.put(read(1));
                        } catch (InterruptedException e) {
                           Thread.currentThread().interrupt();
                           return; // Exit if interrupted
                        }

                        instructionPointer += 2;
                        break;
                    case 5: // Jump-if-true
                        if (read(1) != 0) {
                            instructionPointer = read(2);
                        } else {
                            instructionPointer += 3;
                        }
                        break;
                    case 6: // Jump-if-false
                        if (read(1) == 0) {
                            instructionPointer = read(2);
                        } else {
                            instructionPointer += 3;
                        }
                        break;
                    case 7: // Less than
                        write(3, read(1) < read(2) ? 1 : 0);
                        instructionPointer += 4;
                        break;
                    case 8: // Equals
                        write(3, read(1) == read(2) ? 1 : 0);
                        instructionPointer += 4;
                        break;
                    case 9: // Adjust relative base
                        relativeBase += read(1);
                        instructionPointer += 2;
                        break;
                    case 99: // Halt
                        return;
                    default:
                        throw new IllegalArgumentException("Invalid opcode: " + opcode);
                }
            }
        }

        private long read(int parameterOffset) {
            int mode = (int) (memory[(int) instructionPointer] / (int) Math.pow(10, 1 + parameterOffset)) % 10;
            long address = instructionPointer + parameterOffset;

            switch(mode){
                case 0: //position mode
                    return memory[(int)memory[(int)address]];
                case 1: // immediate mode
                    return memory[(int) address];
                case 2: //relative mode
                   return memory[(int) (relativeBase + memory[(int)address])];
                default:
                    throw new IllegalArgumentException("Invalid parameter mode encountered while reading");

            }
        }

        private void write(int parameterOffset, long value) {

           int mode = (int) (memory[(int) instructionPointer] / (int) Math.pow(10, 1 + parameterOffset)) % 10;
           long address = instructionPointer + parameterOffset;

            switch(mode){
                case 0 : // position
                    memory[(int)memory[(int)address]] = value;
                    break;
                case 2: //relative mode
                   memory[(int)(relativeBase + memory[(int)address])] = value;
                    break;
                default:
                     throw new IllegalArgumentException("Invalid parameter mode encountered while writing");
            }
        }
    }
}
