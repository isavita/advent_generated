
import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Scanner;

public class solution {

    public static void main(String[] args) {
        HashMap<String, Integer> overlaps = new HashMap<>();

        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] parts = line.split(" -> ");
                String[] start = parts[0].split(",");
                String[] end = parts[1].split(",");

                int x1 = Integer.parseInt(start[0]);
                int y1 = Integer.parseInt(start[1]);
                int x2 = Integer.parseInt(end[0]);
                int y2 = Integer.parseInt(end[1]);

                int xStep = sign(x2 - x1);
                int yStep = sign(y2 - y1);
                int steps = abs(x2 - x1) + 1;
                if (Math.abs(y2 - y1) > Math.abs(x2 - x1)) {
                    steps = Math.abs(y2 - y1) + 1;
                }

                for (int i = 0; i < steps; i++) {
                    String point = (x1 + i * xStep) + "," + (y1 + i * yStep);
                    overlaps.put(point, overlaps.getOrDefault(point, 0) + 1);
                }
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }

        int count = 0;
        for (int v : overlaps.values()) {
            if (v > 1) {
                count++;
            }
        }

        System.out.println(count);
    }

    public static int abs(int x) {
        return x < 0 ? -x : x;
    }

    public static int sign(int x) {
        if (x > 0) {
            return 1;
        } else if (x < 0) {
            return -1;
        }
        return 0;
    }
}
