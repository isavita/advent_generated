import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ChronospatialComputer {
    private long registerA;
    private long registerB;
    private long registerC;
    private final List<Integer> program;
    private List<Integer> output;
    private int instructionPointer;
    
    public ChronospatialComputer(List<Integer> program) {
        this.program = new ArrayList<>(program);
        reset();
    }
    
    public void reset() {
        registerA = 0;
        registerB = 0;
        registerC = 0;
        output = new ArrayList<>();
        instructionPointer = 0;
    }
    
    public void setRegisters(long a, long b, long c) {
        this.registerA = a;
        this.registerB = b;
        this.registerC = c;
    }
    
    private int getComboOperandValue(int operand) {
        return switch (operand) {
            case 0, 1, 2, 3 -> operand;
            case 4 -> (int) (registerA & 0x7FFFFFFFL);
            case 5 -> (int) (registerB & 0x7FFFFFFFL);
            case 6 -> (int) (registerC & 0x7FFFFFFFL);
            default -> throw new RuntimeException("Invalid combo operand: " + operand);
        };
    }
    
    public List<Integer> run() {
        while (instructionPointer < program.size()) {
            int cmd = program.get(instructionPointer++);
            
            if (instructionPointer >= program.size()) {
                break; // No operand available
            }
            
            int operand = program.get(instructionPointer++);
            
            switch (cmd) {
                case 0: // adv
                    registerA >>= getComboOperandValue(operand);
                    break;
                case 1: // bxl
                    registerB ^= operand;
                    break;
                case 2: // bst
                    registerB = getComboOperandValue(operand) % 8;
                    break;
                case 3: // jnz
                    if (registerA != 0) {
                        instructionPointer = operand;
                    }
                    break;
                case 4: // bxc
                    registerB ^= registerC;
                    break;
                case 5: // out
                    output.add(getComboOperandValue(operand) % 8);
                    break;
                case 6: // bdv
                    registerB = registerA >> getComboOperandValue(operand);
                    break;
                case 7: // cdv
                    registerC = registerA >> getComboOperandValue(operand);
                    break;
                default:
                    throw new RuntimeException("Invalid opcode: " + cmd);
            }
        }
        
        return output;
    }
    
    // Method that builds up possible A values by working backwards from expected output
    public static List<Long> findValidValues(List<Integer> program, long initialB, long initialC) {
        List<Long> validValues = new ArrayList<>();
        Stack<State> stack = new Stack<>();
        HashSet<State> seen = new HashSet<>();
        
        // Start with depth 0 and score 0
        stack.push(new State(0, 0));
        
        while (!stack.isEmpty()) {
            State current = stack.pop();
            
            if (seen.contains(current)) {
                continue;
            }
            seen.add(current);
            
            // If we've matched the entire program length, we have a valid value
            if (current.depth == program.size()) {
                validValues.add(current.score);
            } else {
                // Try all possible 3-bit values (0-7)
                for (int i = 0; i < 8; i++) {
                    // Build up the new A value
                    long newScore = i + 8 * current.score;
                    
                    ChronospatialComputer computer = new ChronospatialComputer(program);
                    computer.setRegisters(newScore, initialB, initialC);
                    List<Integer> result = computer.run();
                    
                    // Check if the first output matches the expected program element
                    // (working backward from the end)
                    if (!result.isEmpty() && 
                        result.get(0) == program.get(program.size() - 1 - current.depth)) {
                        stack.push(new State(current.depth + 1, newScore));
                    }
                }
            }
        }
        
        return validValues;
    }
    
    private static class State {
        final int depth;
        final long score;
        
        State(int depth, long score) {
            this.depth = depth;
            this.score = score;
        }
        
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            State state = (State) o;
            return depth == state.depth && score == state.score;
        }
        
        @Override
        public int hashCode() {
            return Objects.hash(depth, score);
        }
    }
    
    public static void main(String[] args) {
        try {
            // Read program from file
            InputData inputData = readInputFromFile("input.txt");
            List<Integer> program = inputData.program;
            
            System.out.println("Program: " + program);
            System.out.println("Program size: " + program.size() + " elements");
            System.out.println("Finding valid values...");
            
            List<Long> validValues = findValidValues(program, inputData.initialB, inputData.initialC);
            
            if (validValues.isEmpty()) {
                System.out.println("No solution found.");
            } else {
                long minValue = validValues.stream().min(Long::compare).orElse(0L);
                System.out.println("Lowest positive A value: " + minValue);
            }
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    private static class InputData {
        List<Integer> program;
        long initialA;
        long initialB;
        long initialC;
        
        public InputData() {
            program = new ArrayList<>();
        }
    }
    
    private static InputData readInputFromFile(String filename) throws IOException {
        InputData data = new InputData();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            Pattern registerPattern = Pattern.compile("Register ([A-C]): (\\d+)");
            
            while ((line = reader.readLine()) != null) {
                Matcher m = registerPattern.matcher(line);
                if (m.find()) {
                    String register = m.group(1);
                    long value = Long.parseLong(m.group(2));
                    
                    switch (register) {
                        case "A":
                            data.initialA = value;
                            break;
                        case "B":
                            data.initialB = value;
                            break;
                        case "C":
                            data.initialC = value;
                            break;
                    }
                } else if (line.startsWith("Program:")) {
                    // Extract program part
                    String programPart = line.substring(line.indexOf(":") + 1).trim();
                    String[] values = programPart.split(",");
                    for (String value : values) {
                        data.program.add(Integer.parseInt(value.trim()));
                    }
                }
            }
        }
        return data;
    }
}
