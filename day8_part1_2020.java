
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        List<String> instructions = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                instructions.add(line);
            }
        } catch (IOException e) {
            System.out.println("Error reading file: " + e);
        }

        int[] result = executeBootCode(instructions);
        System.out.println(result[0]);
    }

    public static int[] executeBootCode(List<String> instructions) {
        int accumulator = 0;
        Map<Integer, Boolean> visited = new HashMap<>();
        int currentInstruction = 0;

        while (currentInstruction < instructions.size()) {
            if (visited.containsKey(currentInstruction)) {
                return new int[]{accumulator, 1};
            }

            visited.put(currentInstruction, true);
            String[] parts = instructions.get(currentInstruction).split(" ");
            String op = parts[0];
            int arg = Integer.parseInt(parts[1]);

            switch (op) {
                case "acc":
                    accumulator += arg;
                    currentInstruction++;
                    break;
                case "jmp":
                    currentInstruction += arg;
                    break;
                case "nop":
                    currentInstruction++;
                    break;
            }
        }

        return new int[]{accumulator, 0};
    }
}
