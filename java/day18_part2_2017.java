
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

public class Duet {

    public static void main(String[] args) {
        try {
            List<String> instructions = readInstructions("input.txt");

            // Part 1
            long recoveredFrequency = solvePart1(instructions);
            System.out.println("Part 1 - Recovered frequency: " + recoveredFrequency);

            // Part 2
            long sendCount = solvePart2(instructions);
            System.out.println("Part 2 - Program 1 send count: " + sendCount);

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static List<String> readInstructions(String filename) throws IOException {
        List<String> instructions = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                instructions.add(line);
            }
        }
        return instructions;
    }

    private static long solvePart1(List<String> instructions) {
        Map<String, Long> registers = new HashMap<>();
        long lastSoundPlayed = 0;
        long recoveredFrequency = 0;

        for (int i = 0; i < instructions.size(); i++) {
            String[] parts = instructions.get(i).split(" ");
            String instruction = parts[0];
            String x = parts[1];
            long xValue = getRegisterValue(registers, x);

            switch (instruction) {
                case "snd":
                    lastSoundPlayed = xValue;
                    break;
                case "set":
                    registers.put(x, getRegisterValue(registers, parts[2]));
                    break;
                case "add":
                    registers.put(x, xValue + getRegisterValue(registers, parts[2]));
                    break;
                case "mul":
                    registers.put(x, xValue * getRegisterValue(registers, parts[2]));
                    break;
                case "mod":
                    registers.put(x, xValue % getRegisterValue(registers, parts[2]));
                    break;
                case "rcv":
                    if (xValue != 0) {
                        recoveredFrequency = lastSoundPlayed;
                        return recoveredFrequency;
                    }
                    break;
                case "jgz":
                    if (xValue > 0) {
                        i += (int) getRegisterValue(registers, parts[2]) - 1;
                    }
                    break;
            }
        }

        return recoveredFrequency;
    }

    private static long solvePart2(List<String> instructions) {
        Program program0 = new Program(0, instructions);
        Program program1 = new Program(1, instructions);

        program0.setOtherProgram(program1);
        program1.setOtherProgram(program0);

        while (!program0.isWaiting() || !program1.isWaiting()) {
            program0.run();
            program1.run();
            if (program0.isTerminated() && program1.isTerminated()) break;
        }

        return program1.getSendCount();
    }

    private static long getRegisterValue(Map<String, Long> registers, String registerOrValue) {
        try {
            return Long.parseLong(registerOrValue);
        } catch (NumberFormatException e) {
            return registers.getOrDefault(registerOrValue, 0L);
        }
    }

    static class Program {
        private final int id;
        private final List<String> instructions;
        private final Map<String, Long> registers;
        private final Queue<Long> receiveQueue;
        private Program otherProgram;
        private int instructionPointer;
        private long sendCount;
        private boolean waiting;
        private boolean terminated;

        public Program(int id, List<String> instructions) {
            this.id = id;
            this.instructions = instructions;
            this.registers = new HashMap<>();
            this.registers.put("p", (long) id);
            this.receiveQueue = new LinkedList<>();
            this.instructionPointer = 0;
            this.sendCount = 0;
            this.waiting = false;
            this.terminated = false;
        }

        public void setOtherProgram(Program otherProgram) {
            this.otherProgram = otherProgram;
        }

        public void send(long value) {
            otherProgram.receiveQueue.add(value);
            sendCount++;
        }

        public void run() {
            if (terminated || instructionPointer < 0 || instructionPointer >= instructions.size()) {
                terminated = true;
                return;
            }

            String[] parts = instructions.get(instructionPointer).split(" ");
            String instruction = parts[0];
            String x = parts[1];
            long xValue = getRegisterValue(registers, x);

            switch (instruction) {
                case "snd":
                    send(xValue);
                    instructionPointer++;
                    break;
                case "set":
                    registers.put(x, getRegisterValue(registers, parts[2]));
                    instructionPointer++;
                    break;
                case "add":
                    registers.put(x, xValue + getRegisterValue(registers, parts[2]));
                    instructionPointer++;
                    break;
                case "mul":
                    registers.put(x, xValue * getRegisterValue(registers, parts[2]));
                    instructionPointer++;
                    break;
                case "mod":
                    registers.put(x, xValue % getRegisterValue(registers, parts[2]));
                    instructionPointer++;
                    break;
                case "rcv":
                    if (!receiveQueue.isEmpty()) {
                        registers.put(x, receiveQueue.poll());
                        instructionPointer++;
                        waiting = false;
                    } else {
                        waiting = true;
                        return;
                    }
                    break;
                case "jgz":
                    if (xValue > 0) {
                        instructionPointer += (int) getRegisterValue(registers, parts[2]);
                    } else {
                        instructionPointer++;
                    }
                    break;
            }
            if (instructionPointer < 0 || instructionPointer >= instructions.size()) {
                terminated = true;
            }
        }

        public long getSendCount() {
            return sendCount;
        }

        public boolean isWaiting() {
            return waiting;
        }
        public boolean isTerminated() {
            return terminated;
        }
    }
}
