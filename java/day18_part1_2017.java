
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Duet {

    public static void main(String[] args) {
        try {
            List<String> instructions = readInstructions("input.txt");
            long recoveredFrequency = executeInstructions(instructions);
            System.out.println(recoveredFrequency);
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

    private static long executeInstructions(List<String> instructions) {
        HashMap<String, Long> registers = new HashMap<>();
        long lastSound = 0;
        int instructionPointer = 0;
        Pattern pattern = Pattern.compile("([a-z]+) ([a-z]|-?\\d+) ?([a-z]|-?\\d+)?");

        while (instructionPointer >= 0 && instructionPointer < instructions.size()) {
            String instruction = instructions.get(instructionPointer);
            Matcher matcher = pattern.matcher(instruction);
            if (!matcher.matches()) {
                throw new IllegalArgumentException("Invalid instruction: " + instruction);
            }

            String opcode = matcher.group(1);
            String x = matcher.group(2);
            String y = matcher.group(3);

            switch (opcode) {
                case "snd":
                    lastSound = getValue(registers, x);
                    instructionPointer++;
                    break;
                case "set":
                    registers.put(x, getValue(registers, y));
                    instructionPointer++;
                    break;
                case "add":
                    registers.put(x, registers.getOrDefault(x, 0L) + getValue(registers, y));
                    instructionPointer++;
                    break;
                case "mul":
                    registers.put(x, registers.getOrDefault(x, 0L) * getValue(registers, y));
                    instructionPointer++;
                    break;
                case "mod":
                    registers.put(x, registers.getOrDefault(x, 0L) % getValue(registers, y));
                    instructionPointer++;
                    break;
                case "rcv":
                    if (getValue(registers, x) != 0) {
                        return lastSound;
                    }
                    instructionPointer++;
                    break;
                case "jgz":
                    if (getValue(registers, x) > 0) {
                        instructionPointer += getValue(registers, y);
                    } else {
                        instructionPointer++;
                    }
                    break;
                default:
                    throw new IllegalArgumentException("Unknown opcode: " + opcode);
            }
        }
        return -1; // Should not reach here if the program executes correctly
    }

    private static long getValue(HashMap<String, Long> registers, String value) {
        try {
            return Long.parseLong(value);
        } catch (NumberFormatException e) {
            return registers.getOrDefault(value, 0L);
        }
    }
}
