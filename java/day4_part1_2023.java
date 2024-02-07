import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Scratchcards {

    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);

            int totalPoints = 0;

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] parts = line.split("\\|");

                String[] winningNumbers = parts[0].trim().split("\\s+");
                String[] yourNumbers = parts[1].trim().split("\\s+");

                int points = 0;
                for (String number : yourNumbers) {
                    if (arrayContainsNumber(winningNumbers, number)) {
                        points = points == 0 ? 1 : points * 2;
                    }
                }

                totalPoints += points;
            }

            System.out.println(totalPoints);

            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }

    public static boolean arrayContainsNumber(String[] arr, String targetValue) {
        for (String s : arr) {
            if (s.equals(targetValue)) {
                return true;
            }
        }
        return false;
    }
}