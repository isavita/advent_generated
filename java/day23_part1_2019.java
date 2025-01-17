
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

public class CategorySix {

    static class IntcodeComputer {
        long[] memory;
        int instructionPointer;
        long relativeBase;
        Queue<Long> inputQueue;
        List<Long> output;
        boolean halted;

        public IntcodeComputer(long[] memory) {
            this.memory = Arrays.copyOf(memory, memory.length * 10);
            this.instructionPointer = 0;
            this.relativeBase = 0;
            this.inputQueue = new LinkedList<>();
            this.output = new ArrayList<>();
            this.halted = false;
        }

        public void addInput(long input) {
            inputQueue.add(input);
        }

        public List<Long> run() {
            output.clear();
            while (!halted) {
                long opcode = memory[instructionPointer] % 100;
                if (opcode == 99) {
                    halted = true;
                    break;
                }

                int[] modes = getParameterModes(memory[instructionPointer]);
                switch ((int) opcode) {
                    case 1: // add
                    case 2: // multiply
                        long param1 = getParameterValue(modes[0], instructionPointer + 1);
                        long param2 = getParameterValue(modes[1], instructionPointer + 2);
                        int param3 = (int) memory[instructionPointer + 3];
                        if (modes[2] == 2) {
                            param3 += relativeBase;
                        }
                        memory[param3] = opcode == 1 ? param1 + param2 : param1 * param2;
                        instructionPointer += 4;
                        break;
                    case 3: // input
                        if (inputQueue.isEmpty()) {
                            return output;
                        }
                        int inputParam = (int) memory[instructionPointer + 1];
                        if (modes[0] == 2) {
                            inputParam += relativeBase;
                        }
                        memory[inputParam] = inputQueue.poll();
                        instructionPointer += 2;
                        break;
                    case 4: // output
                        long outputValue = getParameterValue(modes[0], instructionPointer + 1);
                        output.add(outputValue);
                        instructionPointer += 2;
                        break;
                    case 5: // jump-if-true
                        if (getParameterValue(modes[0], instructionPointer + 1) != 0) {
                            instructionPointer = (int) getParameterValue(modes[1], instructionPointer + 2);
                        } else {
                            instructionPointer += 3;
                        }
                        break;
                    case 6: // jump-if-false
                        if (getParameterValue(modes[0], instructionPointer + 1) == 0) {
                            instructionPointer = (int) getParameterValue(modes[1], instructionPointer + 2);
                        } else {
                            instructionPointer += 3;
                        }
                        break;
                    case 7: // less than
                        long param1LessThan = getParameterValue(modes[0], instructionPointer + 1);
                        long param2LessThan = getParameterValue(modes[1], instructionPointer + 2);
                        int param3LessThan = (int) memory[instructionPointer + 3];
                        if (modes[2] == 2) {
                            param3LessThan += relativeBase;
                        }
                        memory[param3LessThan] = param1LessThan < param2LessThan ? 1 : 0;
                        instructionPointer += 4;
                        break;
                    case 8: // equals
                        long param1Equals = getParameterValue(modes[0], instructionPointer + 1);
                        long param2Equals = getParameterValue(modes[1], instructionPointer + 2);
                        int param3Equals = (int) memory[instructionPointer + 3];
                        if (modes[2] == 2) {
                            param3Equals += relativeBase;
                        }
                        memory[param3Equals] = param1Equals == param2Equals ? 1 : 0;
                        instructionPointer += 4;
                        break;
                    case 9: // adjust relative base
                        relativeBase += getParameterValue(modes[0], instructionPointer + 1);
                        instructionPointer += 2;
                        break;
                    default:
                        throw new IllegalArgumentException("Invalid opcode: " + opcode);
                }
            }
            return output;
        }

        private int[] getParameterModes(long instruction) {
            int[] modes = new int[3];
            instruction /= 100;
            for (int i = 0; i < 3; i++) {
                modes[i] = (int) (instruction % 10);
                instruction /= 10;
            }
            return modes;
        }

        private long getParameterValue(int mode, int parameterIndex) {
            long value = memory[parameterIndex];
            if (mode == 0) {
                return memory[(int) value];
            } else if (mode == 1) {
                return value;
            } else if (mode == 2) {
                return memory[(int) (value + relativeBase)];
            }
            throw new IllegalArgumentException("Invalid parameter mode: " + mode);
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
        String line = reader.readLine();
        reader.close();

        long[] initialMemory = Arrays.stream(line.split(",")).mapToLong(Long::parseLong).toArray();
        Map<Integer, IntcodeComputer> computers = new HashMap<>();
        Map<Integer, Queue<List<Long>>> packetQueues = new HashMap<>();

        for (int i = 0; i < 50; i++) {
            IntcodeComputer computer = new IntcodeComputer(initialMemory);
            computer.addInput(i);
            computers.put(i, computer);
            packetQueues.put(i, new LinkedList<>());
        }

        boolean first255Sent = false;
        long first255Y = -1;

        while (!first255Sent) {
            for (int i = 0; i < 50; i++) {
                IntcodeComputer computer = computers.get(i);
                Queue<List<Long>> queue = packetQueues.get(i);

                if (queue.isEmpty()) {
                    computer.addInput(-1);
                } else {
                    List<Long> packet = queue.poll();
                    computer.addInput(packet.get(0));
                    computer.addInput(packet.get(1));
                }

                List<Long> output = computer.run();
                for (int j = 0; j < output.size(); j += 3) {
                    int destination = output.get(j).intValue();
                    long x = output.get(j + 1);
                    long y = output.get(j + 2);

                    if (destination == 255) {
                        first255Sent = true;
                        first255Y = y;
                        break;
                    } else {
                        packetQueues.get(destination).add(List.of(x, y));
                    }
                }
                if (first255Sent) {
                    break;
                }
            }
        }
        System.out.println(first255Y);
    }
}
