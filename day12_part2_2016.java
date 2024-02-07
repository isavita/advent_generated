
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        File file = new File("input.txt");
        try {
            Scanner scanner = new Scanner(file);
            String[] instructions = new String[1000];
            int index = 0;
            while (scanner.hasNextLine()) {
                instructions[index++] = scanner.nextLine();
            }

            Map<String, Integer> registers = new HashMap<>();
            registers.put("a", 0);
            registers.put("b", 0);
            registers.put("c", 1);
            registers.put("d", 0);

            executeInstructions(instructions, registers);

            System.out.println(registers.get("a"));

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public static void executeInstructions(String[] instructions, Map<String, Integer> registers) {
        int i = 0;
        while (i < instructions.length && instructions[i] != null) {
            String[] parts = instructions[i].split(" ");
            switch (parts[0]) {
                case "cpy":
                    int val = getValue(parts[1], registers);
                    registers.put(parts[2], val);
                    i++;
                    break;
                case "inc":
                    registers.put(parts[1], registers.get(parts[1]) + 1);
                    i++;
                    break;
                case "dec":
                    registers.put(parts[1], registers.get(parts[1]) - 1);
                    i++;
                    break;
                case "jnz":
                    int value = getValue(parts[1], registers);
                    if (value != 0) {
                        int jump = Integer.parseInt(parts[2]);
                        i += jump;
                    } else {
                        i++;
                    }
                    break;
            }
        }
    }

    public static int getValue(String s, Map<String, Integer> registers) {
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            return registers.get(s);
        }
    }
}
