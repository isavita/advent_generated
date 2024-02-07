
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        ArrayList<String> instructions = new ArrayList<>();
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                instructions.add(scanner.nextLine());
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        for (int a = 1; ; a++) {
            if (producesClockSignal(a, instructions)) {
                System.out.println(a);
                break;
            }
        }
    }

    public static boolean producesClockSignal(int a, ArrayList<String> instructions) {
        HashMap<String, Integer> registers = new HashMap<>();
        registers.put("a", a);
        registers.put("b", 0);
        registers.put("c", 0);
        registers.put("d", 0);
        int lastOutput = 0;
        int outputCount = 0;

        for (int i = 0; i < instructions.size(); ) {
            String[] parts = instructions.get(i).split(" ");
            switch (parts[0]) {
                case "cpy":
                    int val = getValue(parts[1], registers);
                    registers.put(parts[2], val);
                    break;
                case "inc":
                    registers.put(parts[1], registers.get(parts[1]) + 1);
                    break;
                case "dec":
                    registers.put(parts[1], registers.get(parts[1]) - 1);
                    break;
                case "jnz":
                    int jump = getValue(parts[2], registers);
                    if (getValue(parts[1], registers) != 0) {
                        i += jump;
                        continue;
                    }
                    break;
                case "out":
                    int output = getValue(parts[1], registers);
                    if (output != 0 && output != 1) {
                        return false;
                    }
                    if (outputCount > 0 && output == lastOutput) {
                        return false;
                    }
                    lastOutput = output;
                    outputCount++;
                    if (outputCount > 50) {
                        return true;
                    }
                    break;
            }
            i++;
        }
        return false;
    }

    public static int getValue(String s, HashMap<String, Integer> registers) {
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            return registers.get(s);
        }
    }
}
