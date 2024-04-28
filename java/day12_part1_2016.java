import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Assembunny {
    private int[] registers = new int[4]; // a, b, c, d
    private String[] instructions;

    public static void main(String[] args) {
        Assembunny assembunny = new Assembunny();
        assembunny.run();
    }

    private void run() {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            instructions = reader.lines().toArray(String[]::new);
            executeInstructions();
            System.out.println("Value of register a: " + registers[0]);
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }

    private void executeInstructions() {
        int ip = 0; // instruction pointer
        while (ip < instructions.length) {
            String[] parts = instructions[ip].split(" ");
            String opcode = parts[0];
            switch (opcode) {
                case "cpy":
                    cpy(parts[1], parts[2]);
                    ip++;
                    break;
                case "inc":
                    inc(parts[1]);
                    ip++;
                    break;
                case "dec":
                    dec(parts[1]);
                    ip++;
                    break;
                case "jnz":
                    ip = jnz(parts[1], parts[2], ip);
                    break;
            }
        }
    }

    private void cpy(String x, String y) {
        int value = getValue(x);
        setRegister(y, value);
    }

    private void inc(String x) {
        int value = getRegister(x);
        setRegister(x, value + 1);
    }

    private void dec(String x) {
        int value = getRegister(x);
        setRegister(x, value - 1);
    }

    private int jnz(String x, String y, int ip) {
        int value = getRegister(x);
        if (value != 0) {
            return ip + Integer.parseInt(y);
        } else {
            return ip + 1;
        }
    }

    private int getRegister(String x) {
        switch (x) {
            case "a":
                return registers[0];
            case "b":
                return registers[1];
            case "c":
                return registers[2];
            case "d":
                return registers[3];
            default:
                return Integer.parseInt(x);
        }
    }

    private void setRegister(String x, int value) {
        switch (x) {
            case "a":
                registers[0] = value;
                break;
            case "b":
                registers[1] = value;
                break;
            case "c":
                registers[2] = value;
                break;
            case "d":
                registers[3] = value;
                break;
        }
    }

    private int getValue(String x) {
        if (x.matches("\\d+")) {
            return Integer.parseInt(x);
        } else {
            return getRegister(x);
        }
    }
}