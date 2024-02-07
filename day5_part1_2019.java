
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

public class Solution {

    private static int getMode(int instruction, int position) {
        return instruction / (int) Math.pow(10, position + 1) % 10;
    }

    private static int getParam(int[] program, int pointer, int mode) {
        if (mode == 0) {
            return program[program[pointer]];
        }
        return program[pointer];
    }

    private static int runProgram(int[] program, int input) {
        int output = 0;
        for (int pointer = 0; pointer < program.length; ) {
            int instruction = program[pointer];
            int opcode = instruction % 100;

            switch (opcode) {
                case 1:
                case 2:
                    int param1 = getParam(program, pointer + 1, getMode(instruction, 1));
                    int param2 = getParam(program, pointer + 2, getMode(instruction, 2));
                    int result = opcode == 1 ? param1 + param2 : param1 * param2;
                    program[program[pointer + 3]] = result;
                    pointer += 4;
                    break;
                case 3:
                    program[program[pointer + 1]] = input;
                    pointer += 2;
                    break;
                case 4:
                    output = getParam(program, pointer + 1, getMode(instruction, 1));
                    pointer += 2;
                    break;
                case 99:
                    return output;
                default:
                    throw new RuntimeException("Unknown opcode: " + opcode);
            }
        }
        return output;
    }

    public static void main(String[] args) throws IOException {
        String[] strProgram = Files.readString(Paths.get("input.txt")).trim().split(",");
        int[] program = Arrays.stream(strProgram).mapToInt(Integer::parseInt).toArray();
        System.out.println(runProgram(program, 1));
    }
}
