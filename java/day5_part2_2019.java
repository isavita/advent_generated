
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Solution {

    public static void main(String[] args) {
        List<Integer> program = new ArrayList<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String programStr = reader.readLine();
            String[] instructions = programStr.split(",");
            for (String instruction : instructions) {
                program.add(Integer.parseInt(instruction));
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        int input = 5;
        int output = 0;
        int i = 0;
        while (i < program.size()) {
            int opcode = program.get(i) % 100;
            int modes = program.get(i) / 100;
            int param1Mode = modes % 10;
            modes /= 10;
            int param2Mode = modes % 10;

            switch (opcode) {
                case 1:
                    int p1 = getValue(program, i + 1, param1Mode);
                    int p2 = getValue(program, i + 2, param2Mode);
                    int p3 = program.get(i + 3);
                    program.set(p3, p1 + p2);
                    i += 4;
                    break;
                case 2:
                    p1 = getValue(program, i + 1, param1Mode);
                    p2 = getValue(program, i + 2, param2Mode);
                    p3 = program.get(i + 3);
                    program.set(p3, p1 * p2);
                    i += 4;
                    break;
                case 3:
                    program.set(program.get(i + 1), input);
                    i += 2;
                    break;
                case 4:
                    output = getValue(program, i + 1, param1Mode);
                    System.out.println(output);
                    i += 2;
                    break;
                case 5:
                    p1 = getValue(program, i + 1, param1Mode);
                    p2 = getValue(program, i + 2, param2Mode);
                    if (p1 != 0) {
                        i = p2;
                    } else {
                        i += 3;
                    }
                    break;
                case 6:
                    p1 = getValue(program, i + 1, param1Mode);
                    p2 = getValue(program, i + 2, param2Mode);
                    if (p1 == 0) {
                        i = p2;
                    } else {
                        i += 3;
                    }
                    break;
                case 7:
                    p1 = getValue(program, i + 1, param1Mode);
                    p2 = getValue(program, i + 2, param2Mode);
                    p3 = program.get(i + 3);
                    if (p1 < p2) {
                        program.set(p3, 1);
                    } else {
                        program.set(p3, 0);
                    }
                    i += 4;
                    break;
                case 8:
                    p1 = getValue(program, i + 1, param1Mode);
                    p2 = getValue(program, i + 2, param2Mode);
                    p3 = program.get(i + 3);
                    if (p1 == p2) {
                        program.set(p3, 1);
                    } else {
                        program.set(p3, 0);
                    }
                    i += 4;
                    break;
                case 99:
                    return;
                default:
                    throw new IllegalArgumentException("Invalid opcode");
            }
        }
    }

    private static int getValue(List<Integer> program, int pos, int mode) {
        if (mode == 0) {
            return program.get(program.get(pos));
        } else {
            return program.get(pos);
        }
    }
}
