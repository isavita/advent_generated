import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            int[] positions = new int[7]; // assuming at most 7 discs
            int[] sizes = new int[7];
            int disc = 0;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" ");
                sizes[disc] = Integer.parseInt(parts[3]);
                positions[disc] = Integer.parseInt(parts[11].substring(0, parts[11].length() - 1));
                disc++;
            }

            int time = 0;
            while (true) {
                boolean success = true;
                for (int i = 0; i < disc; i++) {
                    if ((positions[i] + time + i + 1) % sizes[i] != 0) {
                        success = false;
                        break;
                    }
                }
                if (success) {
                    System.out.println("The first time you can press the button is: " + time);
                    break;
                }
                time++;
            }
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }
}