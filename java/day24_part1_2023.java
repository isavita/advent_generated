
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class HailstonePaths {

    static class Hailstone {
        long px, py, vx, vy;

        public Hailstone(long px, long py, long vx, long vy) {
            this.px = px;
            this.py = py;
            this.vx = vx;
            this.vy = vy;
        }
    }

    public static void main(String[] args) {
        String inputFile = "input.txt";
        List<Hailstone> hailstones = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(inputFile))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split(" @ ");
                String[] pos = parts[0].split(", ");
                String[] vel = parts[1].split(", ");
                hailstones.add(new Hailstone(
                        Long.parseLong(pos[0].trim()),
                        Long.parseLong(pos[1].trim()),
                        Long.parseLong(vel[0].trim()),
                        Long.parseLong(vel[1].trim())
                ));
            }
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
            return;
        }

        long minCoord = 200000000000000L;
        long maxCoord = 400000000000000L;
        int intersections = 0;

        for (int i = 0; i < hailstones.size(); i++) {
            for (int j = i + 1; j < hailstones.size(); j++) {
                if (intersect(hailstones.get(i), hailstones.get(j), minCoord, maxCoord)) {
                    intersections++;
                }
            }
        }

        System.out.println(intersections);
    }

    static boolean intersect(Hailstone h1, Hailstone h2, long min, long max) {
        // Solve for t1 and t2:
        // h1.px + t1 * h1.vx = h2.px + t2 * h2.vx
        // h1.py + t1 * h1.vy = h2.py + t2 * h2.vy

        // Rearrange to:
        // t1 * h1.vx - t2 * h2.vx = h2.px - h1.px
        // t1 * h1.vy - t2 * h2.vy = h2.py - h1.py

        // Using Cramer's rule (or direct substitution):
        long det = h1.vx * h2.vy - h1.vy * h2.vx;

        if (det == 0) {
            // Parallel lines
            return false;
        }

        double t1 = (double) (h2.vy * (h2.px - h1.px) - h2.vx * (h2.py - h1.py)) / det;
        double t2 = (double) (h1.vy * (h2.px - h1.px) - h1.vx * (h2.py - h1.py)) / det;

        // Check if intersection is in the future for both hailstones
        if (t1 < 0 || t2 < 0) {
            return false;
        }

        double intersectX = h1.px + t1 * h1.vx;
        double intersectY = h1.py + t1 * h1.vy;

        return intersectX >= min && intersectX <= max && intersectY >= min && intersectY <= max;
    }
}
