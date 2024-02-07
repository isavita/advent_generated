
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        int maxCalories = 0;
        int currentCalories = 0;

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.isEmpty()) {
                    if (currentCalories > maxCalories) {
                        maxCalories = currentCalories;
                    }
                    currentCalories = 0;
                    continue;
                }

                int calories = Integer.parseInt(line);
                currentCalories += calories;
            }

            if (currentCalories > maxCalories) {
                maxCalories = currentCalories;
            }

            System.out.println(maxCalories);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
