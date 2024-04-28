import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Main {
    public static void main(String[] args) throws IOException {
        Map<String, Integer> registers = new HashMap<>();
        registers.put("a", 0);
        registers.put("b", 0);

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            int i = 0;
            String[] instructions = new String[1000]; // assuming max 1000 lines
            while ((line = br.readLine()) != null) {
                instructions[i++] = line;
            }

            for (int j = 0; j < i; j++) {
                String[] parts = instructions[j].trim().split("\\s+");

                switch (parts[0]) {
                    case "hlf":
                        registers.put(parts[1], registers.get(parts[1]) / 2);
                        break;
                    case "tpl":
                        registers.put(parts[1], registers.get(parts[1]) * 3);
                        break;
                    case "inc":
                        registers.put(parts[1], registers.get(parts[1]) + 1);
                        break;
                    case "jmp":
                        j += Integer.parseInt(parts[1]) - 1;
                        break;
                    case "jie":
                        if (registers.get(parts[1].substring(0, 1)) % 2 == 0) {
                            j += Integer.parseInt(parts[2]) - 1;
                        }
                        break;
                    case "jio":
                        if (registers.get(parts[1].substring(0, 1)) == 1) {
                            j += Integer.parseInt(parts[2]) - 1;
                        }
                        break;
                    default:
                        System.out.println("Unknown instruction: " + parts[0]);
                        return;
                }
            }
        }

        System.out.println(registers.get("b"));
    }
}