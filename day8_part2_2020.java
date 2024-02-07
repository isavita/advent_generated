
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        ArrayList<String> instructions = new ArrayList<>();
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                instructions.add(scanner.nextLine());
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e.getMessage());
            return;
        }

        for (int i = 0; i < instructions.size(); i++) {
            String[] parsedInstruction = parseInstruction(instructions.get(i));
            String op = parsedInstruction[0];
            int arg = Integer.parseInt(parsedInstruction[1]);

            if (op.equals("acc")) {
                continue;
            }

            ArrayList<String> modifiedInstructions = new ArrayList<>(instructions);
            if (op.equals("jmp")) {
                modifiedInstructions.set(i, "nop " + arg);
            } else {
                modifiedInstructions.set(i, "jmp " + arg);
            }

            int[] result = executeBootCode(modifiedInstructions);
            int accumulator = result[0];
            boolean terminated = result[1] == 1;

            if (terminated) {
                System.out.println(accumulator);
                break;
            }
        }
    }

    public static int[] executeBootCode(ArrayList<String> instructions) {
        int accumulator = 0;
        boolean[] visited = new boolean[instructions.size()];
        int currentInstruction = 0;

        while (currentInstruction < instructions.size()) {
            if (visited[currentInstruction]) {
                return new int[]{accumulator, 0};
            }

            visited[currentInstruction] = true;
            String[] instruction = parseInstruction(instructions.get(currentInstruction));
            String op = instruction[0];
            int arg = Integer.parseInt(instruction[1]);

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

        return new int[]{accumulator, 1};
    }

    public static String[] parseInstruction(String instruction) {
        String[] parts = instruction.split(" ");
        return new String[]{parts[0], parts[1]};
    }
}
