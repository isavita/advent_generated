
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class AmplifierCircuit {

    public static void main(String[] args) {
        try {
            String intcodeProgram = readInput("input.txt");
            System.out.println("Part 1: " + findMaxThrusterSignal(intcodeProgram, false));
            System.out.println("Part 2: " + findMaxThrusterSignal(intcodeProgram, true));
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static String readInput(String filename) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(filename));
        String line = reader.readLine();
        reader.close();
        return line;
    }

    private static long findMaxThrusterSignal(String program, boolean feedbackLoop) {
        long maxSignal = 0;
        int[] phaseSettings = feedbackLoop ? new int[]{5, 6, 7, 8, 9} : new int[]{0, 1, 2, 3, 4};
        List<List<Integer>> permutations = generatePermutations(phaseSettings);

        for (List<Integer> permutation : permutations) {
            maxSignal = Math.max(maxSignal, runAmplifiers(program, permutation, feedbackLoop));
        }

        return maxSignal;
    }

    private static long runAmplifiers(String program, List<Integer> phaseSettings, boolean feedbackLoop) {
        int numAmplifiers = phaseSettings.size();
        IntcodeComputer[] amplifiers = new IntcodeComputer[numAmplifiers];
        for (int i = 0; i < numAmplifiers; i++) {
            amplifiers[i] = new IntcodeComputer(program);
        }

        long signal = 0;
        boolean firstRound = true;
        int ampIndex = 0;

        while (feedbackLoop ? !amplifiers[numAmplifiers - 1].isHalted() : ampIndex < numAmplifiers) {
            IntcodeComputer currentAmp = amplifiers[ampIndex % numAmplifiers];

            if (firstRound) {
                currentAmp.addInput(phaseSettings.get(ampIndex % numAmplifiers));
            }
            currentAmp.addInput(signal);

            signal = currentAmp.run();
            ampIndex++;
            if (ampIndex >= numAmplifiers) {
                firstRound = false;
            }
        }

        return signal;
    }

    private static List<List<Integer>> generatePermutations(int[] arr) {
        List<List<Integer>> result = new ArrayList<>();
        generatePermutationsHelper(arr, 0, result);
        return result;
    }

    private static void generatePermutationsHelper(int[] arr, int index, List<List<Integer>> result) {
        if (index == arr.length - 1) {
            List<Integer> permutation = new ArrayList<>();
            for (int num : arr) {
                permutation.add(num);
            }
            result.add(permutation);
            return;
        }

        for (int i = index; i < arr.length; i++) {
            swap(arr, index, i);
            generatePermutationsHelper(arr, index + 1, result);
            swap(arr, index, i); // Backtrack
        }
    }

    private static void swap(int[] arr, int i, int j) {
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }
}

class IntcodeComputer {
    private long[] memory;
    private int ip;
    private int relativeBase;
    private List<Long> inputQueue;
    private long output;
    private boolean halted;

    public IntcodeComputer(String program) {
        String[] opcodes = program.split(",");
        this.memory = new long[10000]; // Increased memory size
        for (int i = 0; i < opcodes.length; i++) {
            this.memory[i] = Long.parseLong(opcodes[i]);
        }
        this.ip = 0;
        this.relativeBase = 0;
        this.inputQueue = new ArrayList<>();
        this.output = 0;
        this.halted = false;
    }

    public void addInput(long input) {
        inputQueue.add(input);
    }

    public long run() {
        while (true) {
            int instruction = (int) memory[ip];
            int opcode = instruction % 100;

            if (opcode == 99) {
                halted = true;
                return output;
            }

            int[] modes = new int[3];
            modes[0] = (instruction / 100) % 10;
            modes[1] = (instruction / 1000) % 10;
            modes[2] = (instruction / 10000) % 10;

            if (opcode == 1) { // Add
                long a = readValue(ip + 1, modes[0]);
                long b = readValue(ip + 2, modes[1]);
                writeValue(ip + 3, modes[2], a + b);
                ip += 4;
            } else if (opcode == 2) { // Multiply
                long a = readValue(ip + 1, modes[0]);
                long b = readValue(ip + 2, modes[1]);
                writeValue(ip + 3, modes[2], a * b);
                ip += 4;
            } else if (opcode == 3) { // Input
                if (inputQueue.isEmpty()) {
                    return output; // Wait for input
                }
                writeValue(ip + 1, modes[0], inputQueue.remove(0));
                ip += 2;
            } else if (opcode == 4) { // Output
                output = readValue(ip + 1, modes[0]);
                ip += 2;
                return output;
            } else if (opcode == 5) { // Jump-if-true
                if (readValue(ip + 1, modes[0]) != 0) {
                    ip = (int) readValue(ip + 2, modes[1]);
                } else {
                    ip += 3;
                }
            } else if (opcode == 6) { // Jump-if-false
                if (readValue(ip + 1, modes[0]) == 0) {
                    ip = (int) readValue(ip + 2, modes[1]);
                } else {
                    ip += 3;
                }
            } else if (opcode == 7) { // Less than
                long a = readValue(ip + 1, modes[0]);
                long b = readValue(ip + 2, modes[1]);
                writeValue(ip + 3, modes[2], (a < b) ? 1 : 0);
                ip += 4;
            } else if (opcode == 8) { // Equals
                long a = readValue(ip + 1, modes[0]);
                long b = readValue(ip + 2, modes[1]);
                writeValue(ip + 3, modes[2], (a == b) ? 1 : 0);
                ip += 4;
            } else if (opcode == 9) { // Adjust relative base
                relativeBase += readValue(ip + 1, modes[0]);
                ip += 2;
            } else {
                throw new RuntimeException("Invalid opcode: " + opcode);
            }
        }
    }

    private long readValue(int address, int mode) {
        if (mode == 0) {
            return memory[(int) memory[address]];
        } else if (mode == 1) {
            return memory[address];
        } else {
            return memory[(int) memory[address] + relativeBase];
        }
    }

    private void writeValue(int address, int mode, long value) {
        if (mode == 0) {
            memory[(int) memory[address]] = value;
        } else if (mode == 2) {
            memory[(int) memory[address] + relativeBase] = value;
        } else {
            throw new RuntimeException("Invalid mode for write operation");
        }
    }

    public boolean isHalted() {
        return halted;
    }
}
