
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class Day23 {

    public static void main(String[] args) throws IOException {
        String input = new String(Files.readAllBytes(Paths.get("input.txt")));
        long[] program = Arrays.stream(input.split(",")).mapToLong(Long::parseLong).toArray();

        System.out.println("Part 1: " + part1(program));
        System.out.println("Part 2: " + part2(program));
    }

    static long part1(long[] program) {
        IntcodeComputer[] computers = new IntcodeComputer[50];
        Queue<Long>[] queues = new Queue[50];
        for (int i = 0; i < 50; i++) {
            computers[i] = new IntcodeComputer(program.clone());
            computers[i].addInput(i);
            queues[i] = new LinkedList<>();
        }

        while (true) {
            for (int i = 0; i < 50; i++) {
                if (queues[i].isEmpty()) {
                    computers[i].addInput(-1);
                } else {
                    computers[i].addInput(queues[i].poll());
                    computers[i].addInput(queues[i].poll());
                }

                List<Long> output = computers[i].run();
                for (int j = 0; j < output.size(); j += 3) {
                    long address = output.get(j);
                    long x = output.get(j + 1);
                    long y = output.get(j + 2);
                    if (address == 255) {
                        return y;
                    }
                    queues[(int) address].add(x);
                    queues[(int) address].add(y);
                }
            }
        }
    }

    static long part2(long[] program) {
        IntcodeComputer[] computers = new IntcodeComputer[50];
        Queue<Long>[] queues = new Queue[50];
        for (int i = 0; i < 50; i++) {
            computers[i] = new IntcodeComputer(program.clone());
            computers[i].addInput(i);
            queues[i] = new LinkedList<>();
        }

        long natX = 0, natY = 0;
        long lastDeliveredY = -1;
        long idleCount = 0;

        while (true) {
            boolean idle = true;
            for (int i = 0; i < 50; i++) {
                if (queues[i].isEmpty()) {
                    computers[i].addInput(-1);
                } else {
                    idle = false;
                    computers[i].addInput(queues[i].poll());
                    computers[i].addInput(queues[i].poll());
                }

                List<Long> output = computers[i].run();
                for (int j = 0; j < output.size(); j += 3) {
                    idle = false;
                    long address = output.get(j);
                    long x = output.get(j + 1);
                    long y = output.get(j + 2);
                    if (address == 255) {
                        natX = x;
                        natY = y;
                    } else {
                        queues[(int) address].add(x);
                        queues[(int) address].add(y);
                    }
                }
            }

            if (idle) {
                idleCount++;
                if (idleCount > 100) { 
                    if (natY == lastDeliveredY) {
                        return natY;
                    }
                    queues[0].add(natX);
                    queues[0].add(natY);
                    lastDeliveredY = natY;
                    idleCount = 0;
                }
            } else {
                idleCount = 0;
            }
        }
    }
}

class IntcodeComputer {
    long[] memory;
    int pointer = 0;
    int relativeBase = 0;
    Queue<Long> inputQueue = new LinkedList<>();
    List<Long> output = new ArrayList<>();

    public IntcodeComputer(long[] program) {
        this.memory = new long[10000];
        System.arraycopy(program, 0, this.memory, 0, program.length);
    }

    public void addInput(long input) {
        inputQueue.add(input);
    }

    public List<Long> run() {
        output.clear();
        while (true) {
            int instruction = (int) memory[pointer];
            int opcode = instruction % 100;
            int mode1 = (instruction / 100) % 10;
            int mode2 = (instruction / 1000) % 10;
            int mode3 = (instruction / 10000) % 10;

            if (opcode == 99) {
                break;
            }

            long param1, param2;
            switch (opcode) {
                case 1:
                case 2:
                case 7:
                case 8:
                    param1 = getParam(mode1, pointer + 1);
                    param2 = getParam(mode2, pointer + 2);
                    int writeAddress = (int) getWriteAddress(mode3, pointer + 3);
                    if (opcode == 1) {
                        memory[writeAddress] = param1 + param2;
                    } else if (opcode == 2) {
                        memory[writeAddress] = param1 * param2;
                    } else if (opcode == 7) {
                        memory[writeAddress] = (param1 < param2) ? 1 : 0;
                    } else {
                        memory[writeAddress] = (param1 == param2) ? 1 : 0;
                    }
                    pointer += 4;
                    break;
                case 3:
                    if (inputQueue.isEmpty()) {
                        return output;
                    }
                    memory[(int) getWriteAddress(mode1, pointer + 1)] = inputQueue.poll();
                    pointer += 2;
                    break;
                case 4:
                    output.add(getParam(mode1, pointer + 1));
                    pointer += 2;
                    break;
                case 5:
                    param1 = getParam(mode1, pointer + 1);
                    param2 = getParam(mode2, pointer + 2);
                    if (param1 != 0) {
                        pointer = (int) param2;
                    } else {
                        pointer += 3;
                    }
                    break;
                case 6:
                    param1 = getParam(mode1, pointer + 1);
                    param2 = getParam(mode2, pointer + 2);
                    if (param1 == 0) {
                        pointer = (int) param2;
                    } else {
                        pointer += 3;
                    }
                    break;
                case 9:
                    relativeBase += getParam(mode1, pointer + 1);
                    pointer += 2;
                    break;
                default:
                    throw new RuntimeException("Invalid opcode: " + opcode);
            }
        }
        return output;
    }

    private long getParam(int mode, int address) {
        switch (mode) {
            case 0:
                return memory[(int) memory[address]];
            case 1:
                return memory[address];
            case 2:
                return memory[relativeBase + (int) memory[address]];
            default:
                throw new RuntimeException("Invalid parameter mode: " + mode);
        }
    }

    private long getWriteAddress(int mode, int address) {
        if (mode == 2) {
            return relativeBase + memory[address];
        } else {
            return memory[address];
        }
    }
}
