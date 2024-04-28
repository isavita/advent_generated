import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {

    private static int[] phases = {0, 1, 2, 3, 4};
    private static int maxOutput = 0;

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String line = br.readLine();
        String[] program = line.split(",");
        int[] memory = new int[program.length];
        for (int i = 0; i < program.length; i++) {
            memory[i] = Integer.parseInt(program[i]);
        }

        permute(phases, 0, memory.clone());

        System.out.println("Max thruster signal: " + maxOutput);
    }

    private static void permute(int[] phases, int start, int[] memory) {
        if (start == phases.length) {
            int output = 0;
            for (int i = 0; i < phases.length; i++) {
                int[] inputs = new int[]{phases[i], output};
                output = runProgram(memory.clone(), inputs);
            }
            maxOutput = Math.max(maxOutput, output);
        } else {
            for (int i = start; i < phases.length; i++) {
                swap(phases, start, i);
                permute(phases, start + 1, memory);
                swap(phases, start, i);
            }
        }
    }

    private static void swap(int[] phases, int i, int j) {
        int temp = phases[i];
        phases[i] = phases[j];
        phases[j] = temp;
    }

    private static int runProgram(int[] memory, int[] inputs) {
        int ip = 0;
        int inputIndex = 0;
        while (ip < memory.length) {
            int opcode = memory[ip] % 100;
            int[] modes = getModes(memory[ip] / 100);
            int[] params = new int[3];
            for (int i = 0; i < 3; i++) {
                if (modes[i] == 0) {
                    params[i] = memory[ip + i + 1];
                } else {
                    params[i] = ip + i + 1;
                }
            }
            switch (opcode) {
                case 1:
                    memory[params[2]] = memory[params[0]] + memory[params[1]];
                    ip += 4;
                    break;
                case 2:
                    memory[params[2]] = memory[params[0]] * memory[params[1]];
                    ip += 4;
                    break;
                case 3:
                    memory[params[0]] = inputs[inputIndex++];
                    ip += 2;
                    break;
                case 4:
                    ip += 2;
                    return memory[params[0]];
                case 5:
                    if (memory[params[0]] != 0) {
                        ip = memory[params[1]];
                    } else {
                        ip += 3;
                    }
                    break;
                case 6:
                    if (memory[params[0]] == 0) {
                        ip = memory[params[1]];
                    } else {
                        ip += 3;
                    }
                    break;
                case 7:
                    if (memory[params[0]] < memory[params[1]]) {
                        memory[params[2]] = 1;
                    } else {
                        memory[params[2]] = 0;
                    }
                    ip += 4;
                    break;
                case 8:
                    if (memory[params[0]] == memory[params[1]]) {
                        memory[params[2]] = 1;
                    } else {
                        memory[params[2]] = 0;
                    }
                    ip += 4;
                    break;
                case 99:
                    return inputs[1];
            }
        }
        return 0;
    }

    private static int[] getModes(int modes) {
        int[] result = new int[3];
        for (int i = 0; i < 3; i++) {
            result[i] = modes % 10;
            modes /= 10;
        }
        return result;
    }
}