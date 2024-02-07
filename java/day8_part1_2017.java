
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            Map<String, Integer> registers = new HashMap<>();

            while (scanner.hasNextLine()) {
                String[] parts = scanner.nextLine().split(" ");
                String reg = parts[0];
                String op = parts[1];
                int amount = Integer.parseInt(parts[2]);
                String condReg = parts[4];
                String condOp = parts[5];
                int condVal = Integer.parseInt(parts[6]);

                boolean cond = false;
                switch (condOp) {
                    case ">":
                        cond = registers.getOrDefault(condReg, 0) > condVal;
                        break;
                    case ">=":
                        cond = registers.getOrDefault(condReg, 0) >= condVal;
                        break;
                    case "<":
                        cond = registers.getOrDefault(condReg, 0) < condVal;
                        break;
                    case "<=":
                        cond = registers.getOrDefault(condReg, 0) <= condVal;
                        break;
                    case "==":
                        cond = registers.getOrDefault(condReg, 0) == condVal;
                        break;
                    case "!=":
                        cond = registers.getOrDefault(condReg, 0) != condVal;
                        break;
                }

                if (cond) {
                    switch (op) {
                        case "inc":
                            registers.put(reg, registers.getOrDefault(reg, 0) + amount);
                            break;
                        case "dec":
                            registers.put(reg, registers.getOrDefault(reg, 0) - amount);
                            break;
                    }
                }
            }

            int maxValue = 0;
            for (int value : registers.values()) {
                if (value > maxValue) {
                    maxValue = value;
                }
            }

            System.out.println(maxValue);
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("File reading error: " + e.getMessage());
        }
    }
}
