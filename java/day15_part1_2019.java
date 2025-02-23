
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Droid {
    private final IntcodeComputer computer;
    private final Map<Integer, int[]> directionMap;
    private final Map<Point, Integer> grid;
    private Point currentPosition;

    public Droid(long[] program) {
        this.computer = new IntcodeComputer(program);
        this.directionMap = new HashMap<>();
        this.directionMap.put(1, new int[]{0, -1});
        this.directionMap.put(2, new int[]{0, 1});
        this.directionMap.put(3, new int[]{-1, 0});
        this.directionMap.put(4, new int[]{1, 0});
        this.currentPosition = new Point(0, 0);
        this.grid = new HashMap<>();
        this.grid.put(this.currentPosition, 1);
    }

    public static void main(String[] args) throws IOException {
        long[] program = parseInput("input.txt");
        Droid droid = new Droid(program);
        int steps = droid.explore();
        System.out.println(steps);
    }

    private static long[] parseInput(String filePath) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(filePath));
        String line = reader.readLine();
        reader.close();
        String[] parts = line.split(",");
        long[] program = new long[parts.length];
        for (int i = 0; i < parts.length; i++) {
            program[i] = Long.parseLong(parts[i]);
        }
        return program;
    }

    private int sendMoveCommand(int direction) {
        computer.addInput(direction);
        computer.run();
        return (int) computer.getOutput();
    }

    private int explore() {
        Queue<State> queue = new LinkedList<>();
        queue.add(new State(currentPosition, 0));
        Set<Point> visited = new HashSet<>();
        visited.add(currentPosition);

        while (!queue.isEmpty()) {
            State currentState = queue.poll();
            moveTo(currentState.position);

            for (int direction : directionMap.keySet()) {
                int[] delta = directionMap.get(direction);
                Point newPos = new Point(currentState.position.x + delta[0], currentState.position.y + delta[1]);

                if (visited.contains(newPos)) continue;

                int status = sendMoveCommand(direction);
                if (status == 0) {
                    grid.put(newPos, 0);
                } else {
                    grid.put(newPos, (status == 1) ? 1 : 2);
                    visited.add(newPos);
                    if (status == 2) {
                      moveTo(newPos);
                      return currentState.steps + 1;
                    }
                    queue.add(new State(newPos, currentState.steps + 1));
                    sendMoveCommand(getOppositeDirection(direction));
                }
            }
        }
        return -1;
    }

    private int getOppositeDirection(int direction) {
        return switch (direction) {
            case 1 -> 2;
            case 2 -> 1;
            case 3 -> 4;
            case 4 -> 3;
            default -> throw new IllegalArgumentException("Invalid direction: " + direction);
        };
    }

    private void moveTo(Point target) {
        List<Integer> path = findPath(currentPosition, target);
        for (int direction : path) {
            sendMoveCommand(direction);
            int[] delta = directionMap.get(direction);
            currentPosition = new Point(currentPosition.x + delta[0], currentPosition.y + delta[1]);
        }
    }

    private List<Integer> findPath(Point start, Point end) {
        Queue<PathState> queue = new LinkedList<>();
        queue.add(new PathState(start, new ArrayList<>()));
        Set<Point> visited = new HashSet<>();
        visited.add(start);

        while (!queue.isEmpty()) {
            PathState currentState = queue.poll();
            if (currentState.position.equals(end)) {
                return currentState.path;
            }

            for (int direction : directionMap.keySet()) {
                int[] delta = directionMap.get(direction);
                Point newPos = new Point(currentState.position.x + delta[0], currentState.position.y + delta[1]);
                if (visited.contains(newPos) || grid.getOrDefault(newPos, 0) == 0) continue;
                visited.add(newPos);
                List<Integer> newPath = new ArrayList<>(currentState.path);
                newPath.add(direction);
                queue.add(new PathState(newPos, newPath));
            }
        }
        throw new RuntimeException("No path found from " + start + " to " + end);
    }

    static class IntcodeComputer {
        private final Map<Long, Long> memory;
        private long ip;
        private long relativeBase;
        private final Queue<Long> inputQueue;
        private final Queue<Long> outputQueue;

        public IntcodeComputer(long[] program) {
            this.memory = new HashMap<>();
            for (long i = 0; i < program.length; i++) {
                this.memory.put(i, program[(int) i]);
            }
            this.ip = 0;
            this.relativeBase = 0;
            this.inputQueue = new LinkedList<>();
            this.outputQueue = new LinkedList<>();
        }

        public void addInput(long value) {
            inputQueue.add(value);
        }

        public long getOutput() {
            return outputQueue.poll();
        }

        private long getParameter(int mode, int offset) {
            long param = memory.getOrDefault(ip + offset, 0L);
            return switch (mode) {
                case 0 -> memory.getOrDefault(param, 0L);
                case 1 -> param;
                case 2 -> memory.getOrDefault(relativeBase + param, 0L);
                default -> throw new IllegalArgumentException("Unknown parameter mode: " + mode);
            };
        }

        private void setParameter(int mode, int offset, long value) {
            long param = memory.getOrDefault(ip + offset, 0L);
            switch (mode) {
                case 0 -> memory.put(param, value);
                case 2 -> memory.put(relativeBase + param, value);
                default -> throw new IllegalArgumentException("Unknown parameter mode for writing: " + mode);
            }
        }

        public void run() {
            while (true) {
                long instruction = memory.getOrDefault(ip, 0L);
                int opcode = (int) (instruction % 100);
                int[] modes = {
                        (int) ((instruction / 100) % 10),
                        (int) ((instruction / 1000) % 10),
                        (int) ((instruction / 10000) % 10)
                };

                switch (opcode) {
                    case 1 -> {
                        long param1 = getParameter(modes[0], 1);
                        long param2 = getParameter(modes[1], 2);
                        setParameter(modes[2], 3, param1 + param2);
                        ip += 4;
                    }
                    case 2 -> {
                        long param1 = getParameter(modes[0], 1);
                        long param2 = getParameter(modes[1], 2);
                        setParameter(modes[2], 3, param1 * param2);
                        ip += 4;
                    }
                    case 3 -> {
                        setParameter(modes[0], 1, inputQueue.poll());
                        ip += 2;
                    }
                    case 4 -> {
                        outputQueue.add(getParameter(modes[0], 1));
                        ip += 2;
                        return;
                    }
                    case 5 -> {
                        long param1 = getParameter(modes[0], 1);
                        long param2 = getParameter(modes[1], 2);
                        ip = (param1 != 0) ? param2 : ip + 3;
                    }
                    case 6 -> {
                        long param1 = getParameter(modes[0], 1);
                        long param2 = getParameter(modes[1], 2);
                        ip = (param1 == 0) ? param2 : ip + 3;
                    }
                    case 7 -> {
                        long param1 = getParameter(modes[0], 1);
                        long param2 = getParameter(modes[1], 2);
                        setParameter(modes[2], 3, (param1 < param2) ? 1 : 0);
                        ip += 4;
                    }
                    case 8 -> {
                        long param1 = getParameter(modes[0], 1);
                        long param2 = getParameter(modes[1], 2);
                        setParameter(modes[2], 3, (param1 == param2) ? 1 : 0);
                        ip += 4;
                    }
                    case 9 -> {
                        relativeBase += getParameter(modes[0], 1);
                        ip += 2;
                    }
                    case 99 -> {
                        return;
                    }
                    default -> throw new IllegalArgumentException("Unknown opcode: " + opcode);
                }
            }
        }
    }

    record Point(int x, int y) {
    }

    record State(Point position, int steps) {
    }

    record PathState(Point position, List<Integer> path) {
    }
}
