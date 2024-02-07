
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.awt.Point;

public class solution {
    private static final Point[] Neighbors4 = {new Point(0, 1), new Point(0, -1), new Point(1, 0), new Point(-1, 0)};

    public static void main(String[] args) {
        HashMap<Point, Integer> grid = new HashMap<>();
        HashMap<Point, Boolean> visible = new HashMap<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            int y = 0;
            while ((line = reader.readLine()) != null) {
                char[] chars = line.toCharArray();
                for (int x = 0; x < chars.length; x++) {
                    grid.put(new Point(x, y), chars[x] - '0');
                }
                y++;
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        for (Point p : grid.keySet()) {
            for (Point n : Neighbors4) {
                Point next = new Point(p);
                while (true) {
                    next.translate(n.x, n.y);
                    if (grid.containsKey(next)) {
                        if (grid.get(next) >= grid.get(p)) {
                            break;
                        }
                    } else {
                        visible.put(p, true);
                        break;
                    }
                }
            }
        }
        System.out.println(visible.size());
    }
}
