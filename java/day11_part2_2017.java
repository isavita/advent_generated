
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            String input = scanner.nextLine();
            String[] directions = input.split(",");

            int x = 0, y = 0, z = 0;
            int maxDistance = 0;

            for (String dir : directions) {
                switch (dir) {
                    case "n":
                        y++;
                        z--;
                        break;
                    case "ne":
                        x++;
                        z--;
                        break;
                    case "se":
                        x++;
                        y--;
                        break;
                    case "s":
                        y--;
                        z++;
                        break;
                    case "sw":
                        x--;
                        z++;
                        break;
                    case "nw":
                        x--;
                        y++;
                        break;
                }

                int curDistance = (Math.abs(x) + Math.abs(y) + Math.abs(z)) / 2;
                maxDistance = Math.max(maxDistance, curDistance);
            }

            System.out.println(maxDistance);

        } catch (FileNotFoundException e) {
            System.out.println("File reading error" + e);
        }
    }
}
