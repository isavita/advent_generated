
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class TuringLock {

    public static void main(String[] args) throws IOException {
        List<String> instructions = readInstructions("input.txt");

        System.out.println("Part 1: " + runProgram(instructions, 0));
        System.out.println("Part 2: " + runProgram(instructions, 1));
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

    private static long runProgram(List<String> instructions, int initialA) {
        long a = initialA;
        long b = 0;
        int ip = 0;

        while (ip >= 0 && ip < instructions.size()) {
            String[] parts = instructions.get(ip).split(" ");
            String instruction = parts[0];

            switch (instruction) {
                case "hlf":
                    if (parts[1].equals("a")) {
                        a /= 2;
                    } else {
                        b /= 2;
                    }
                    ip++;
                    break;
                case "tpl":
                    if (parts[1].equals("a")) {
                        a *= 3;
                    } else {
                        b *= 3;
                    }
                    ip++;
                    break;
                case "inc":
                    if (parts[1].equals("a")) {
                        a++;
                    } else {
                        b++;
                    }
                    ip++;
                    break;
                case "jmp":
                    ip += Integer.parseInt(parts[1]);
                    break;
                case "jie":
                    long regValueJie = (parts[1].charAt(0) == 'a') ? a : b;
                    if (regValueJie % 2 == 0) {
                        ip += Integer.parseInt(parts[2].substring(1));
                    } else {
                        ip++;
                    }
                    break;
                case "jio":
                    long regValueJio = (parts[1].charAt(0) == 'a') ? a : b;
                    if (regValueJio == 1) {
                        ip += Integer.parseInt(parts[2].substring(1));
                    } else {
                        ip++;
                    }
                    break;
            }
        }
        return b;
    }
}
