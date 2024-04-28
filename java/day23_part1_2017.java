import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String[] instructions = br.lines().toArray(String[]::new);
        br.close();

        int mulCount = 0;
        int pointer = 0;
        Map<String, Integer> registers = new HashMap<>();

        while (pointer >= 0 && pointer < instructions.length) {
            String[] parts = instructions[pointer].split(" ");
            String cmd = parts[0];
            String x = parts[1];
            String y = parts.length > 2 ? parts[2] : "0";

            int getValue = isNumber(x) ? Integer.parseInt(x) : registers.getOrDefault(x, 0);

            switch (cmd) {
                case "set":
                    registers.put(x, isNumber(y) ? Integer.parseInt(y) : registers.getOrDefault(y, 0));
                    break;
                case "sub":
                    registers.put(x, registers.getOrDefault(x, 0) - (isNumber(y) ? Integer.parseInt(y) : registers.getOrDefault(y, 0)));
                    break;
                case "mul":
                    registers.put(x, registers.getOrDefault(x, 0) * (isNumber(y) ? Integer.parseInt(y) : registers.getOrDefault(y, 0)));
                    mulCount++;
                    break;
                case "jnz":
                    if (getValue != 0) {
                        pointer += (isNumber(y) ? Integer.parseInt(y) : registers.getOrDefault(y, 0)) - 1;
                    }
                    break;
            }
            pointer++;
        }

        System.out.println(mulCount);
    }

    private static boolean isNumber(String s) {
        try {
            Integer.parseInt(s);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }
}