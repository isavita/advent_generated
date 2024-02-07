
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

public class solution {
    public static void main(String[] args) {
        try {
            // Step 1: Read Input
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            
            // Step 2: Initialize Registers
            HashMap<String, Integer> registers = new HashMap<>();
            
            // Initialize highest value
            int highestValue = 0;
            
            // Step 3: Process Instructions
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(" ");
                String reg = parts[0];
                String op = parts[1];
                int amount = Integer.parseInt(parts[2]);
                String condReg = parts[4];
                String condOp = parts[5];
                int condVal = Integer.parseInt(parts[6]);
                
                // Check condition
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
                    
                    // Update highest value
                    if (registers.get(reg) > highestValue) {
                        highestValue = registers.get(reg);
                    }
                }
            }
            
            // Step 4: Print the highest value
            System.out.println(highestValue);
            
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
