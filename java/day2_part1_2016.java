
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Solution {

    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);

            int[][] keypad = {
                    {1, 2, 3},
                    {4, 5, 6},
                    {7, 8, 9}
            };
            int x = 1;
            int y = 1;

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                for (char c : line.toCharArray()) {
                    if (c == 'U' && y > 0) {
                        y--;
                    } else if (c == 'D' && y < 2) {
                        y++;
                    } else if (c == 'L' && x > 0) {
                        x--;
                    } else if (c == 'R' && x < 2) {
                        x++;
                    }
                }
                System.out.print(keypad[y][x]);
            }

            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
