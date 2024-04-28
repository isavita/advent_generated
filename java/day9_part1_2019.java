import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class IntcodeComputer {

    private long[] memory;
    private int pointer;
    private int relativeBase;

    public IntcodeComputer(String filename) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(filename));
        String line = reader.readLine();
        String[] parts = line.split(",");
        memory = new long[10000]; // initialize memory with a large size
        for (int i = 0; i < parts.length; i++) {
            memory[i] = Long.parseLong(parts[i]);
        }
        pointer = 0;
        relativeBase = 0;
    }

    private long getParameter(int mode, int offset) {
        long value;
        if (mode == 0) { // position mode
            value = memory[(int) memory[pointer + offset]];
        } else if (mode == 1) { // immediate mode
            value = memory[pointer + offset];
        } else { // relative mode
            value = memory[(int) (memory[pointer + offset] + relativeBase)];
        }
        return value;
    }

    private void setParameter(int mode, int offset, long value) {
        if (mode == 0) { // position mode
            memory[(int) memory[pointer + offset]] = value;
        } else { // relative mode
            memory[(int) (memory[pointer + offset] + relativeBase)] = value;
        }
    }

    private void run() {
        while (true) {
            int opcode = (int) memory[pointer] % 100;
            int mode1 = (int) memory[pointer] / 100 % 10;
            int mode2 = (int) memory[pointer] / 1000 % 10;
            int mode3 = (int) memory[pointer] / 10000 % 10;

            if (opcode == 1) { // add
                long param1 = getParameter(mode1, 1);
                long param2 = getParameter(mode2, 2);
                setParameter(mode3, 3, param1 + param2);
                pointer += 4;
            } else if (opcode == 2) { // multiply
                long param1 = getParameter(mode1, 1);
                long param2 = getParameter(mode2, 2);
                setParameter(mode3, 3, param1 * param2);
                pointer += 4;
            } else if (opcode == 3) { // input
                // read input from user
                setParameter(mode1, 1, 1); // for test mode, provide input 1
                pointer += 2;
            } else if (opcode == 4) { // output
                long param1 = getParameter(mode1, 1);
                System.out.println(param1);
                pointer += 2;
            } else if (opcode == 5) { // jump if true
                long param1 = getParameter(mode1, 1);
                long param2 = getParameter(mode2, 2);
                if (param1 != 0) {
                    pointer = (int) param2;
                } else {
                    pointer += 3;
                }
            } else if (opcode == 6) { // jump if false
                long param1 = getParameter(mode1, 1);
                long param2 = getParameter(mode2, 2);
                if (param1 == 0) {
                    pointer = (int) param2;
                } else {
                    pointer += 3;
                }
            } else if (opcode == 7) { // less than
                long param1 = getParameter(mode1, 1);
                long param2 = getParameter(mode2, 2);
                setParameter(mode3, 3, param1 < param2 ? 1 : 0);
                pointer += 4;
            } else if (opcode == 8) { // equals
                long param1 = getParameter(mode1, 1);
                long param2 = getParameter(mode2, 2);
                setParameter(mode3, 3, param1 == param2 ? 1 : 0);
                pointer += 4;
            } else if (opcode == 9) { // adjust relative base
                long param1 = getParameter(mode1, 1);
                relativeBase += param1;
                pointer += 2;
            } else if (opcode == 99) { // halt
                break;
            } else {
                System.out.println("Error: unknown opcode " + opcode);
                break;
            }
        }
    }

    public static void main(String[] args) throws IOException {
        IntcodeComputer computer = new IntcodeComputer("input.txt");
        computer.run();
    }
}