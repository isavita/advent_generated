
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            int target = Integer.parseInt(scanner.nextLine().trim());
            scanner.close();

            int sideLength = (int) Math.ceil(Math.sqrt(target));
            if (sideLength % 2 == 0) {
                sideLength++;
            }

            int maxValue = sideLength * sideLength;
            int stepsFromEdge = (sideLength - 1) / 2;
            int distanceToMiddle = 0;

            for (int i = 0; i < 4; i++) {
                int middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i;
                int distance = Math.abs(target - middlePoint);
                if (distance < distanceToMiddle || i == 0) {
                    distanceToMiddle = distance;
                }
            }

            int manhattanDistance = stepsFromEdge + distanceToMiddle;
            System.out.println(manhattanDistance);

        } catch (FileNotFoundException e) {
            System.out.println("File reading error: " + e.getMessage());
        }
    }
}
