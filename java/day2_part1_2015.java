
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);

            int total = 0;
            while (scanner.hasNextLine()) {
                String[] dimensions = scanner.nextLine().split("x");
                if (dimensions.length != 3) {
                    throw new IllegalArgumentException("Invalid input format");
                }

                int l = Integer.parseInt(dimensions[0]);
                int w = Integer.parseInt(dimensions[1]);
                int h = Integer.parseInt(dimensions[2]);

                int side1 = l * w;
                int side2 = w * h;
                int side3 = h * l;

                int smallest = Math.min(Math.min(side1, side2), side3);
                total += 2 * side1 + 2 * side2 + 2 * side3 + smallest;
            }

            System.out.println(total);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
