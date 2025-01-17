
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class SafeCracking {

    public static void main(String[] args) {
        try {
            List<String> instructions = readInstructions("input.txt");
            int result1 = runProgram(instructions, 7);
            System.out.println("Part 1: " + result1);

            int result2 = runProgram(instructions, 12);
            System.out.println("Part 2: " + result2);

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

    private static int runProgram(List<String> instructions, int initialA) {
        int[] registers = new int[4];
        registers[0] = initialA;
        List<String[]> parsedInstructions = new ArrayList<>();
        for (String instruction : instructions) {
            parsedInstructions.add(instruction.split(" "));
        }

        int ip = 0;
        while (ip >= 0 && ip < parsedInstructions.size()) {
            String[] instruction = parsedInstructions.get(ip);
            String opcode = instruction[0];

            switch (opcode) {
                case "cpy":
                    int src = getValue(instruction[1], registers);
                    if (isRegister(instruction[2])) {
                        registers[instruction[2].charAt(0) - 'a'] = src;
                    }
                    ip++;
                    break;
                case "inc":
                    if (isRegister(instruction[1])) {
                        registers[instruction[1].charAt(0) - 'a']++;
                    }
                    ip++;
                    break;
                case "dec":
                    if (isRegister(instruction[1])) {
                        registers[instruction[1].charAt(0) - 'a']--;
                    }
                    ip++;
                    break;
                case "jnz":
                    int val = getValue(instruction[1], registers);
                    if (val != 0) {
                        ip += getValue(instruction[2], registers);
                    } else {
                        ip++;
                    }
                    break;
                case "tgl":
                    int offset = getValue(instruction[1], registers);
                    int target = ip + offset;
                    if (target >= 0 && target < parsedInstructions.size()) {
                        toggleInstruction(parsedInstructions.get(target));
                    }
                    ip++;
                    break;
                default:
                    ip++;
                    break;
            }
        }
        return registers[0];
    }

    private static void toggleInstruction(String[] instruction) {
        if (instruction.length == 2) {
            if (instruction[0].equals("inc")) {
                instruction[0] = "dec";
            } else {
                instruction[0] = "inc";
            }
        } else if (instruction.length == 3) {
            if (instruction[0].equals("jnz")) {
                instruction[0] = "cpy";
            } else {
                instruction[0] = "jnz";
            }
        }
    }

    private static int getValue(String arg, int[] registers) {
        if (isRegister(arg)) {
            return registers[arg.charAt(0) - 'a'];
        } else {
            return Integer.parseInt(arg);
        }
    }

    private static boolean isRegister(String arg) {
        return arg.length() == 1 && arg.charAt(0) >= 'a' && arg.charAt(0) <= 'd';
    }
}
