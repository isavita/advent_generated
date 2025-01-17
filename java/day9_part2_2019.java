
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class Intcode {

    private static long[] memory;
    private static int relativeBase = 0;
    private static int instructionPointer = 0;

    public static void main(String[] args) throws FileNotFoundException {
        File file = new File("input.txt");
        Scanner scanner = new Scanner(file);
        String line = scanner.nextLine();
        String[] parts = line.split(",");
        memory = new long[parts.length * 10];
        for (int i = 0; i < parts.length; i++) {
            memory[i] = Long.parseLong(parts[i]);
        }
        scanner.close();

        System.out.println("Part 1: " + runIntcode(1));
        System.out.println("Part 2: " + runIntcode(2));
    }

    private static long runIntcode(int input) {
        instructionPointer = 0;
        relativeBase = 0;
        long output = 0;
        while (true) {
            int opcode = (int) (memory[instructionPointer] % 100);
            int mode1 = (int) (memory[instructionPointer] / 100 % 10);
            int mode2 = (int) (memory[instructionPointer] / 1000 % 10);
            int mode3 = (int) (memory[instructionPointer] / 10000 % 10);

            switch (opcode) {
                case 1:
                    long val1 = getParameterValue(instructionPointer + 1, mode1);
                    long val2 = getParameterValue(instructionPointer + 2, mode2);
                    setParameterValue(instructionPointer + 3, mode3, val1 + val2);
                    instructionPointer += 4;
                    break;
                case 2:
                    val1 = getParameterValue(instructionPointer + 1, mode1);
                    val2 = getParameterValue(instructionPointer + 2, mode2);
                    setParameterValue(instructionPointer + 3, mode3, val1 * val2);
                    instructionPointer += 4;
                    break;
                case 3:
                    setParameterValue(instructionPointer + 1, mode1, input);
                    instructionPointer += 2;
                    break;
                case 4:
                    output = getParameterValue(instructionPointer + 1, mode1);
                    instructionPointer += 2;
                    break;
                case 5:
                    if (getParameterValue(instructionPointer + 1, mode1) != 0) {
                        instructionPointer = (int) getParameterValue(instructionPointer + 2, mode2);
                    } else {
                        instructionPointer += 3;
                    }
                    break;
                case 6:
                    if (getParameterValue(instructionPointer + 1, mode1) == 0) {
                        instructionPointer = (int) getParameterValue(instructionPointer + 2, mode2);
                    } else {
                        instructionPointer += 3;
                    }
                    break;
                case 7:
                    val1 = getParameterValue(instructionPointer + 1, mode1);
                    val2 = getParameterValue(instructionPointer + 2, mode2);
                    setParameterValue(instructionPointer + 3, mode3, val1 < val2 ? 1 : 0);
                    instructionPointer += 4;
                    break;
                case 8:
                    val1 = getParameterValue(instructionPointer + 1, mode1);
                    val2 = getParameterValue(instructionPointer + 2, mode2);
                    setParameterValue(instructionPointer + 3, mode3, val1 == val2 ? 1 : 0);
                    instructionPointer += 4;
                    break;
                case 9:
                    relativeBase += getParameterValue(instructionPointer + 1, mode1);
                    instructionPointer += 2;
                    break;
                case 99:
                    return output;
                default:
                    throw new IllegalArgumentException("Invalid opcode: " + opcode);
            }
        }
    }

    private static long getParameterValue(int address, int mode) {
        long value = memory[address];
        switch (mode) {
            case 0:
                return memory[(int) value];
            case 1:
                return value;
            case 2:
                return memory[(int) (relativeBase + value)];
            default:
                throw new IllegalArgumentException("Invalid mode: " + mode);
        }
    }

    private static void setParameterValue(int address, int mode, long value) {
        long targetAddress = memory[address];
        switch (mode) {
            case 0:
                memory[(int) targetAddress] = value;
                break;
            case 2:
                memory[(int) (relativeBase + targetAddress)] = value;
                break;
            default:
                throw new IllegalArgumentException("Invalid mode for write: " + mode);
        }
    }
}
