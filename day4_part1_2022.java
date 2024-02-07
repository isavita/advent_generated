
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            int count = 0;

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] ranges = line.split(",");
                if (ranges.length != 2) {
                    continue;
                }

                String[] range1 = ranges[0].split("-");
                String[] range2 = ranges[1].split("-");
                int start1 = Integer.parseInt(range1[0]);
                int end1 = Integer.parseInt(range1[1]);
                int start2 = Integer.parseInt(range2[0]);
                int end2 = Integer.parseInt(range2[1]);

                if ((start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1)) {
                    count++;
                }
            }

            System.out.println(count);
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e);
        }
    }
}
