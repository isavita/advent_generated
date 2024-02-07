import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class script {

    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);

            int possibleTriangles = 0;

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] sides = line.trim().split("\\s+");

                int side1 = Integer.parseInt(sides[0]);
                int side2 = Integer.parseInt(sides[1]);
                int side3 = Integer.parseInt(sides[2]);

                if (side1 + side2 > side3 && side1 + side3 > side2 && side2 + side3 > side1) {
                    possibleTriangles++;
                }
            }

            System.out.println(possibleTriangles);

            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}