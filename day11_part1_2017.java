
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String input = reader.readLine();
            reader.close();

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

                int curDistance = distance(x, y, z);
                maxDistance = Math.max(maxDistance, curDistance);
            }

            System.out.println(distance(x, y, z));
        } catch (IOException e) {
            System.out.println("File reading error: " + e.getMessage());
        }
    }

    public static int abs(int x) {
        return x < 0 ? -x : x;
    }

    public static int max(int a, int b) {
        return a > b ? a : b;
    }

    public static int distance(int x, int y, int z) {
        return (abs(x) + abs(y) + abs(z)) / 2;
    }
}
