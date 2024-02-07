
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line = reader.readLine();
            String[] fishStrs = line.split(",");
            int[] fishes = new int[9];
            for (String fishStr : fishStrs) {
                int fish = Integer.parseInt(fishStr);
                fishes[fish]++;
            }

            for (int day = 1; day <= 80; day++) {
                int newFish = fishes[0];
                for (int i = 1; i < fishes.length; i++) {
                    fishes[i-1] = fishes[i];
                }
                fishes[6] += newFish;
                fishes[8] = newFish;
            }

            int totalFish = 0;
            for (int fish : fishes) {
                totalFish += fish;
            }

            System.out.println(totalFish);
            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
        }
    }
}
