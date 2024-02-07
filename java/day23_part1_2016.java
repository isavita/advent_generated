
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        Map<String, Integer> registers = new HashMap<>();
        registers.put("a", 7);
        registers.put("b", 0);
        registers.put("c", 0);
        registers.put("d", 0);

        String[] instructions = readInstructions("input.txt");
        executeInstructions(instructions, registers);

        System.out.println(registers.get("a"));
    }

    public static String[] readInstructions(String filename) {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            return br.lines().toArray(String[]::new);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return new String[0];
    }

    public static void executeInstructions(String[] instructions, Map<String, Integer> registers) {
        int pc = 0;
        while (pc < instructions.length) {
            String[] fields = instructions[pc].split(" ");
            switch (fields[0]) {
                case "cpy":
                    int x = getValue(fields[1], registers);
                    if (registers.containsKey(fields[2])) {
                        registers.put(fields[2], x);
                    }
                    break;
                case "inc":
                    if (registers.containsKey(fields[1])) {
                        registers.put(fields[1], registers.get(fields[1]) + 1);
                    }
                    break;
                case "dec":
                    if (registers.containsKey(fields[1])) {
                        registers.put(fields[1], registers.get(fields[1]) - 1);
                    }
                    break;
                case "jnz":
                    int val = getValue(fields[1], registers);
                    if (val != 0) {
                        pc += getValue(fields[2], registers) - 1;
                    }
                    break;
                case "tgl":
                    int xVal = getValue(fields[1], registers);
                    int tgt = pc + xVal;
                    if (tgt >= 0 && tgt < instructions.length) {
                        instructions[tgt] = toggleInstruction(instructions[tgt]);
                    }
                    break;
            }
            pc++;
        }
    }

    public static int getValue(String s, Map<String, Integer> registers) {
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            return registers.getOrDefault(s, 0);
        }
    }

    public static String toggleInstruction(String instr) {
        String[] parts = instr.split(" ");
        switch (parts[0]) {
            case "inc":
                parts[0] = "dec";
                break;
            case "dec":
            case "tgl":
                parts[0] = "inc";
                break;
            case "jnz":
                parts[0] = "cpy";
                break;
            case "cpy":
                parts[0] = "jnz";
                break;
        }
        return String.join(" ", parts);
    }
}
