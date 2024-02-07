
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {

    public static void main(String[] args) {
        int[] original = readInput("input.txt");

        for (int noun = 0; noun <= 99; noun++) {
            for (int verb = 0; verb <= 99; verb++) {
                int[] memory = original.clone();
                memory[1] = noun;
                memory[2] = verb;
                if (execute(memory) == 19690720) {
                    System.out.println(100 * noun + verb);
                    return;
                }
            }
        }
    }

    public static int execute(int[] memory) {
        for (int i = 0; i < memory.length; i += 4) {
            switch (memory[i]) {
                case 1:
                    memory[memory[i + 3]] = memory[memory[i + 1]] + memory[memory[i + 2]];
                    break;
                case 2:
                    memory[memory[i + 3]] = memory[memory[i + 1]] * memory[memory[i + 2]];
                    break;
                case 99:
                    return memory[0];
            }
        }
        return memory[0];
    }

    public static int[] readInput(String fileName) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(fileName));
            String line = reader.readLine().trim();
            String[] strs = line.split(",");
            int[] original = new int[strs.length];
            for (int i = 0; i < strs.length; i++) {
                original[i] = Integer.parseInt(strs[i]);
            }
            return original;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}
