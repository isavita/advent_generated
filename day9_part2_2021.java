
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

public class solution {

    public static void main(String[] args) {
        List<List<Integer>> heightmap = new ArrayList<>();
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                List<Integer> row = new ArrayList<>();
                for (char c : line.toCharArray()) {
                    int height = Character.getNumericValue(c);
                    row.add(height);
                }
                heightmap.add(row);
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        List<Integer> basinSizes = new ArrayList<>();
        Map<List<Integer>, Boolean> visited = new HashMap<>();

        for (int y = 0; y < heightmap.size(); y++) {
            for (int x = 0; x < heightmap.get(y).size(); x++) {
                if (isLowPoint(heightmap, x, y)) {
                    int size = exploreBasin(heightmap, x, y, visited);
                    basinSizes.add(size);
                }
            }
        }

        Collections.sort(basinSizes, Collections.reverseOrder());
        int result = basinSizes.get(0) * basinSizes.get(1) * basinSizes.get(2);
        System.out.println(result);
    }

    public static boolean isLowPoint(List<List<Integer>> heightmap, int x, int y) {
        int height = heightmap.get(y).get(x);
        if (x > 0 && heightmap.get(y).get(x - 1) <= height) {
            return false;
        }
        if (x < heightmap.get(y).size() - 1 && heightmap.get(y).get(x + 1) <= height) {
            return false;
        }
        if (y > 0 && heightmap.get(y - 1).get(x) <= height) {
            return false;
        }
        if (y < heightmap.size() - 1 && heightmap.get(y + 1).get(x) <= height) {
            return false;
        }
        return true;
    }

    public static int exploreBasin(List<List<Integer>> heightmap, int x, int y, Map<List<Integer>, Boolean> visited) {
        List<Integer> point = new ArrayList<>();
        point.add(x);
        point.add(y);
        if (visited.containsKey(point) || heightmap.get(y).get(x) == 9) {
            return 0;
        }
        visited.put(point, true);
        int size = 1;

        int[][] directions = {{0, -1}, {-1, 0}, {0, 1}, {1, 0}};
        for (int[] dir : directions) {
            int newX = x + dir[0];
            int newY = y + dir[1];
            if (newX >= 0 && newX < heightmap.get(0).size() && newY >= 0 && newY < heightmap.size()) {
                size += exploreBasin(heightmap, newX, newY, visited);
            }
        }
        return size;
    }
}
