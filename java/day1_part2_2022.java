
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            List<Integer> caloriesList = new ArrayList<>();
            int currentCalories = 0;

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();

                if (line.equals("")) {
                    caloriesList.add(currentCalories);
                    currentCalories = 0;
                    continue;
                }

                int calories = Integer.parseInt(line);
                currentCalories += calories;
            }

            caloriesList.add(currentCalories);
            Collections.sort(caloriesList, Collections.reverseOrder());

            int topThreeSum = 0;
            for (int i = 0; i < 3 && i < caloriesList.size(); i++) {
                topThreeSum += caloriesList.get(i);
            }

            System.out.println(topThreeSum);

            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
