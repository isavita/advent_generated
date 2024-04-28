import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Main {

    static class Claim {
        int id;
        int x;
        int y;
        int width;
        int height;

        public Claim(int id, int x, int y, int width, int height) {
            this.id = id;
            this.x = x;
            this.y = y;
            this.width = width;
            this.height = height;
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        File file = new File("input.txt");
        Scanner scanner = new Scanner(file);

        List<Claim> claims = new ArrayList<>();
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            String[] parts = line.split(" ");
            int id = Integer.parseInt(parts[0].substring(1));
            String[] coordinates = parts[2].split(",");
            int x = Integer.parseInt(coordinates[0]);
            int y = Integer.parseInt(coordinates[1].substring(0, coordinates[1].length() - 1));
            String[] size = parts[3].split("x");
            int width = Integer.parseInt(size[0]);
            int height = Integer.parseInt(size[1]);

            claims.add(new Claim(id, x, y, width, height));
        }

        int[][] fabric = new int[1000][1000];
        for (Claim claim : claims) {
            for (int i = claim.x; i < claim.x + claim.width; i++) {
                for (int j = claim.y; j < claim.y + claim.height; j++) {
                    fabric[i][j]++;
                }
            }
        }

        int overlap = 0;
        for (int i = 0; i < 1000; i++) {
            for (int j = 0; j < 1000; j++) {
                if (fabric[i][j] > 1) {
                    overlap++;
                }
            }
        }

        System.out.println("Part 1: " + overlap);

        for (Claim claim : claims) {
            boolean overlaps = false;
            for (int i = claim.x; i < claim.x + claim.width; i++) {
                for (int j = claim.y; j < claim.y + claim.height; j++) {
                    if (fabric[i][j] > 1) {
                        overlaps = true;
                        break;
                    }
                }
                if (overlaps) {
                    break;
                }
            }
            if (!overlaps) {
                System.out.println("Part 2: " + claim.id);
                break;
            }
        }
    }
}