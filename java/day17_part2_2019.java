
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class VacuumRobot {

    static class IntcodeComputer {
        Map<Long, Long> memory = new HashMap<>();
        long pointer = 0;
        long relativeBase = 0;
        Queue<Long> inputs = new LinkedList<>();
        Queue<Long> outputs = new LinkedList<>();
        boolean halted = false;

        IntcodeComputer(long[] program) {
            for (int i = 0; i < program.length; i++) {
                memory.put((long) i, program[i]);
            }
        }

        long getParam(long mode, long param) {
            if (mode == 0) {
                return memory.getOrDefault(param, 0L);
            } else if (mode == 1) {
                return param;
            } else if (mode == 2) {
                return memory.getOrDefault(relativeBase + param, 0L);
            }
            throw new RuntimeException("Unknown parameter mode: " + mode);
        }

        void setParam(long mode, long param, long value) {
            if (mode == 0) {
                memory.put(param, value);
            } else if (mode == 2) {
                memory.put(relativeBase + param, value);
            } else {
                throw new RuntimeException("Unknown parameter mode for writing: " + mode);
            }
        }

        void run() {
            while (true) {
                long instruction = memory.getOrDefault(pointer, 0L);
                long opcode = instruction % 100;
                long[] modes = {
                        (instruction / 100) % 10,
                        (instruction / 1000) % 10,
                        (instruction / 10000) % 10
                };

                if (opcode == 1) {
                    long param1 = memory.getOrDefault(pointer + 1, 0L);
                    long param2 = memory.getOrDefault(pointer + 2, 0L);
                    long param3 = memory.getOrDefault(pointer + 3, 0L);
                    long val1 = getParam(modes[0], param1);
                    long val2 = getParam(modes[1], param2);
                    setParam(modes[2], param3, val1 + val2);
                    pointer += 4;
                } else if (opcode == 2) {
                    long param1 = memory.getOrDefault(pointer + 1, 0L);
                    long param2 = memory.getOrDefault(pointer + 2, 0L);
                    long param3 = memory.getOrDefault(pointer + 3, 0L);
                    long val1 = getParam(modes[0], param1);
                    long val2 = getParam(modes[1], param2);
                    setParam(modes[2], param3, val1 * val2);
                    pointer += 4;
                } else if (opcode == 3) {
                    if (inputs.isEmpty()) {
                        return;
                    }
                    long param1 = memory.getOrDefault(pointer + 1, 0L);
                    long inputValue = inputs.poll();
                    setParam(modes[0], param1, inputValue);
                    pointer += 2;
                } else if (opcode == 4) {
                    long param1 = memory.getOrDefault(pointer + 1, 0L);
                    long outputValue = getParam(modes[0], param1);
                    outputs.add(outputValue);
                    pointer += 2;
                } else if (opcode == 5) {
                    long param1 = memory.getOrDefault(pointer + 1, 0L);
                    long param2 = memory.getOrDefault(pointer + 2, 0L);
                    long val1 = getParam(modes[0], param1);
                    long val2 = getParam(modes[1], param2);
                    if (val1 != 0) {
                        pointer = val2;
                    } else {
                        pointer += 3;
                    }
                } else if (opcode == 6) {
                    long param1 = memory.getOrDefault(pointer + 1, 0L);
                    long param2 = memory.getOrDefault(pointer + 2, 0L);
                    long val1 = getParam(modes[0], param1);
                    long val2 = getParam(modes[1], param2);
                    if (val1 == 0) {
                        pointer = val2;
                    } else {
                        pointer += 3;
                    }
                } else if (opcode == 7) {
                    long param1 = memory.getOrDefault(pointer + 1, 0L);
                    long param2 = memory.getOrDefault(pointer + 2, 0L);
                    long param3 = memory.getOrDefault(pointer + 3, 0L);
                    long val1 = getParam(modes[0], param1);
                    long val2 = getParam(modes[1], param2);
                    setParam(modes[2], param3, (val1 < val2) ? 1 : 0);
                    pointer += 4;
                } else if (opcode == 8) {
                    long param1 = memory.getOrDefault(pointer + 1, 0L);
                    long param2 = memory.getOrDefault(pointer + 2, 0L);
                    long param3 = memory.getOrDefault(pointer + 3, 0L);
                    long val1 = getParam(modes[0], param1);
                    long val2 = getParam(modes[1], param2);
                    setParam(modes[2], param3, (val1 == val2) ? 1 : 0);
                    pointer += 4;
                } else if (opcode == 9) {
                    long param1 = memory.getOrDefault(pointer + 1, 0L);
                    long val1 = getParam(modes[0], param1);
                    relativeBase += val1;
                    pointer += 2;
                } else if (opcode == 99) {
                    halted = true;
                    return;
                } else {
                    throw new RuntimeException("Unknown opcode: " + opcode);
                }
            }
        }
    }

    static char[][] parseMap(Queue<Long> output) {
        List<char[]> gridList = new ArrayList<>();
        List<Character> line = new ArrayList<>();
        for (Long c : output) {
            if (c == 10) {
                if (!line.isEmpty()) {
                    char[] lineArray = new char[line.size()];
                    for (int i = 0; i < line.size(); i++) {
                        lineArray[i] = line.get(i);
                    }
                    gridList.add(lineArray);
                    line.clear();
                }
            } else {
                line.add((char) c.intValue());
            }
        }
        if (!line.isEmpty()) {
            char[] lineArray = new char[line.size()];
            for (int i = 0; i < line.size(); i++) {
                lineArray[i] = line.get(i);
            }
            gridList.add(lineArray);
        }
        char[][] grid = new char[gridList.size()][];
        for (int i = 0; i < gridList.size(); i++) {
            grid[i] = gridList.get(i);
        }
        return grid;
    }

    static List<int[]> findIntersections(char[][] grid) {
        List<int[]> intersections = new ArrayList<>();
        for (int y = 1; y < grid.length - 1; y++) {
            for (int x = 1; x < grid[0].length - 1; x++) {
                if (grid[y][x] != '#') {
                    continue;
                }
                if (grid[y - 1][x] == '#' && grid[y + 1][x] == '#' &&
                        grid[y][x - 1] == '#' && grid[y][x + 1] == '#') {
                    intersections.add(new int[]{x, y});
                }
            }
        }
        return intersections;
    }

    static int[] findRobotPosition(char[][] grid) {
        for (int y = 0; y < grid.length; y++) {
            for (int x = 0; x < grid[y].length; x++) {
                if ("^v<>X".indexOf(grid[y][x]) != -1) {
                    return new int[]{x, y, grid[y][x]};
                }
            }
        }
        return null;
    }

    static char turnLeft(char direction) {
        switch (direction) {
            case '^': return '<';
            case '<': return 'v';
            case 'v': return '>';
            case '>': return '^';
            default: throw new RuntimeException("Invalid direction: " + direction);
        }
    }

    static char turnRight(char direction) {
        switch (direction) {
            case '^': return '>';
            case '>': return 'v';
            case 'v': return '<';
            case '<': return '^';
            default: throw new RuntimeException("Invalid direction: " + direction);
        }
    }

    static int[] moveForward(int x, int y, char direction) {
        switch (direction) {
            case '^': return new int[]{x, y - 1};
            case 'v': return new int[]{x, y + 1};
            case '<': return new int[]{x - 1, y};
            case '>': return new int[]{x + 1, y};
            default: throw new RuntimeException("Invalid direction: " + direction);
        }
    }
    static List<String> getMovementPath(char[][] grid, int startX, int startY, char startDir) {
        int x = startX, y = startY;
        char direction = startDir;
        List<String> path = new ArrayList<>();
        int steps = 0;

        while (true) {
            int[] nextPos = moveForward(x, y, direction);
            int nextX = nextPos[0], nextY = nextPos[1];

            if (nextY >= 0 && nextY < grid.length && nextX >= 0 && nextX < grid[0].length && grid[nextY][nextX] == '#') {
                x = nextX;
                y = nextY;
                steps++;
            } else {
                if (steps > 0) {
                    path.add(String.valueOf(steps));
                    steps = 0;
                }

                char leftDir = turnLeft(direction);
                nextPos = moveForward(x, y, leftDir);
                nextX = nextPos[0];
                nextY = nextPos[1];
                if (nextY >= 0 && nextY < grid.length && nextX >= 0 && nextX < grid[0].length && grid[nextY][nextX] == '#') {
                    path.add("L");
                    direction = leftDir;
                    continue;
                }

                char rightDir = turnRight(direction);
                nextPos = moveForward(x, y, rightDir);
                nextX = nextPos[0];
                nextY = nextPos[1];
                if (nextY >= 0 && nextY < grid.length && nextX >= 0 && nextX < grid[0].length && grid[nextY][nextX] == '#') {
                    path.add("R");
                    direction = rightDir;
                    continue;
                }
                break;
            }
        }
        return path;
    }

    static String[] compressMovement(List<String> path) {
    String pathStr = String.join(",", path);
    String[] tokens = pathStr.split(",");

    int maxFunctionLength = 20;
    int maxPatternLength = 10;

    for (int aLen = 1; aLen <= maxPatternLength; aLen++) {
        String[] aPattern = Arrays.copyOfRange(tokens, 0, aLen);
        String aStr = String.join(",", aPattern);
        if (aStr.length() > maxFunctionLength) continue;
        String[] tokensAfterA = replaceSequence(tokens, aPattern, "A");

        for (int bStart = 0; bStart < tokens.length; bStart++) {
            for (int bLen = 1; bLen <= maxPatternLength; bLen++) {
                if (bStart + bLen > tokens.length) continue;
                String[] bPattern = Arrays.copyOfRange(tokens, bStart, bStart + bLen);
                String bStr = String.join(",", bPattern);
                if (bStr.length() > maxFunctionLength) continue;

                String[] tokensAfterB = replaceSequence(tokensAfterA, bPattern, "B");

                for (int cStart = 0; cStart < tokens.length; cStart++) {
                    for (int cLen = 1; cLen <= maxPatternLength; cLen++) {
                        if (cStart + cLen > tokens.length) continue;
                        String[] cPattern = Arrays.copyOfRange(tokens, cStart, cStart + cLen);
                        String cStr = String.join(",", cPattern);
                        if (cStr.length() > maxFunctionLength) continue;

                        String[] tokensAfterC = replaceSequence(tokensAfterB, cPattern, "C");

                        String[] mainTokens = tokensAfterC;

                        String mainRoutine = String.join(",", mainTokens);
                        mainRoutine = mainRoutine.replaceAll("A,A,A,A", "A");
                        mainRoutine = mainRoutine.replaceAll("A,A,A", "A");
                        mainRoutine = mainRoutine.replaceAll("A,A", "A");

                        mainRoutine = mainRoutine.replaceAll("B,B,B,B", "B");
                        mainRoutine = mainRoutine.replaceAll("B,B,B", "B");
                        mainRoutine = mainRoutine.replaceAll("B,B", "B");

                        mainRoutine = mainRoutine.replaceAll("C,C,C,C", "C");
                        mainRoutine = mainRoutine.replaceAll("C,C,C", "C");
                        mainRoutine = mainRoutine.replaceAll("C,C", "C");



                        if (mainRoutine.matches("[ABC,]+") && mainRoutine.length() <= 20) {
                            String functionA = String.join(",", aPattern);
                            String functionB = String.join(",", bPattern);
                            String functionC = String.join(",", cPattern);

                            if (functionA.length() <= 20 && functionB.length() <= 20 && functionC.length() <= 20) {
                                return new String[]{mainRoutine, functionA, functionB, functionC};
                            }
                        }
                    }
                }
            }
        }
    }
    throw new RuntimeException("Could not compress the path");
}
    static String[] replaceSequence(String[] seq, String[] pattern, String replacement) {
        List<String> res = new ArrayList<>();
        int i = 0;
        while (i < seq.length) {
            boolean match = true;
            for (int j = 0; j < pattern.length; j++) {
                if (i + j >= seq.length || !seq[i + j].equals(pattern[j])) {
                    match = false;
                    break;
                }
            }
            if (match) {
                res.add(replacement);
                i += pattern.length;
            } else {
                res.add(seq[i]);
                i++;
            }
        }
        return res.toArray(new String[0]);
    }
    public static void main(String[] args) throws IOException {
        String input = new String(Files.readAllBytes(Paths.get("input.txt"))).trim();
        long[] program = Arrays.stream(input.split(",")).mapToLong(Long::parseLong).toArray();

        IntcodeComputer computer = new IntcodeComputer(program.clone());
        computer.run();
        char[][] grid = parseMap(computer.outputs);
        List<int[]> intersections = findIntersections(grid);
        int alignmentSum = intersections.stream().mapToInt(p -> p[0] * p[1]).sum();
        System.out.println("Part One: Sum of alignment parameters = " + alignmentSum);

        program[0] = 2;
        IntcodeComputer computer2 = new IntcodeComputer(program.clone());
        int[] robot = findRobotPosition(grid);
        List<String> movementPath = getMovementPath(grid, robot[0], robot[1], (char) robot[2]);

        String[] compressed = compressMovement(movementPath);
        String mainRoutine = compressed[0];
        String functionA = compressed[1];
        String functionB = compressed[2];
        String functionC = compressed[3];


        List<Long> movementInputs = new ArrayList<>();

        for(char c: mainRoutine.toCharArray()) movementInputs.add((long)c);
        movementInputs.add(10L);
        for(char c: functionA.toCharArray()) movementInputs.add((long)c);
        movementInputs.add(10L);
        for(char c: functionB.toCharArray()) movementInputs.add((long)c);
        movementInputs.add(10L);
        for(char c: functionC.toCharArray()) movementInputs.add((long)c);
        movementInputs.add(10L);
        movementInputs.add((long)'n');
        movementInputs.add(10L);

        computer2.inputs.addAll(movementInputs);

        while (!computer2.halted) {
            computer2.run();
        }

        long dustCollected = 0;
        while (!computer2.outputs.isEmpty()) {
            dustCollected = computer2.outputs.poll();
        }
        System.out.println("Part Two: Dust collected = " + dustCollected);
    }
}
