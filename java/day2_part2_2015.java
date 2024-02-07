
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.Arrays;

public class solution {
    public static void main(String[] args) {
        int totalRibbon = 0;
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String[] dimensions = scanner.nextLine().split("x");
                if (dimensions.length != 3) {
                    System.err.println("Invalid input format");
                    System.exit(1);
                }

                int l = Integer.parseInt(dimensions[0]);
                int w = Integer.parseInt(dimensions[1]);
                int h = Integer.parseInt(dimensions[2]);

                // Calculate ribbon for the bow
                int bow = l * w * h;

                // Calculate ribbon for wrapping (smallest perimeter)
                int[] sides = new int[]{l, w, h};
                Arrays.sort(sides);
                int wrap = 2 * sides[0] + 2 * sides[1];

                totalRibbon += bow + wrap;
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            System.err.println(e.getMessage());
            System.exit(1);
        }

        System.out.println(totalRibbon);
    }
}
