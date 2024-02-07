
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class solution {
    static class Asteroid {
        int x, y;
        double angle;
        double dist;

        public Asteroid(int x, int y, double angle, double dist) {
            this.x = x;
            this.y = y;
            this.angle = angle;
            this.dist = dist;
        }
    }

    public static void main(String[] args) {
        List<boolean[]> asteroids = readAsteroids("input.txt");
        int[] station = findBestAsteroidLocation(asteroids);
        List<Asteroid> vaporized = vaporizeAsteroids(asteroids, station);

        if (vaporized.size() >= 200) {
            int result = vaporized.get(199).x * 100 + vaporized.get(199).y;
            System.out.println(result);
        } else {
            System.out.println("Less than 200 asteroids were vaporized.");
        }
    }

    public static List<boolean[]> readAsteroids(String filename) {
        List<boolean[]> asteroids = new ArrayList<>();
        try {
            File file = new File(filename);
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                boolean[] asteroidRow = new boolean[line.length()];
                for (int i = 0; i < line.length(); i++) {
                    asteroidRow[i] = line.charAt(i) == '#';
                }
                asteroids.add(asteroidRow);
            }
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return asteroids;
    }

    public static List<Asteroid> vaporizeAsteroids(List<boolean[]> asteroids, int[] station) {
        List<Asteroid> targets = new ArrayList<>();
        for (int y = 0; y < asteroids.size(); y++) {
            for (int x = 0; x < asteroids.get(y).length; x++) {
                if (asteroids.get(y)[x] && !(x == station[0] && y == station[1])) {
                    double angle = Math.atan2(y - station[1], x - station[0]);
                    double dist = Math.hypot(x - station[0], y - station[1]);
                    if (angle < -Math.PI / 2) {
                        angle += 2 * Math.PI; // Adjust angle for clockwise rotation
                    }
                    targets.add(new Asteroid(x, y, angle, dist));
                }
            }
        }

        targets.sort((a, b) -> {
            if (a.angle == b.angle) {
                return Double.compare(a.dist, b.dist);
            }
            return Double.compare(a.angle, b.angle);
        });

        List<Asteroid> vaporized = new ArrayList<>();
        double lastAngle = -Double.MAX_VALUE;
        for (int i = 0; i < targets.size(); ) {
            if (targets.get(i).angle != lastAngle) {
                vaporized.add(targets.get(i));
                lastAngle = targets.get(i).angle;
                targets.remove(i);
            } else {
                i++;
            }
        }
        return vaporized;
    }

    public static int[] findBestAsteroidLocation(List<boolean[]> asteroids) {
        int[] bestLocation = new int[2];
        int maxCount = 0;
        for (int y = 0; y < asteroids.size(); y++) {
            for (int x = 0; x < asteroids.get(y).length; x++) {
                if (asteroids.get(y)[x]) {
                    int count = countVisibleAsteroids(asteroids, x, y);
                    if (count > maxCount) {
                        maxCount = count;
                        bestLocation[0] = x;
                        bestLocation[1] = y;
                    }
                }
            }
        }
        return bestLocation;
    }

    public static int countVisibleAsteroids(List<boolean[]> asteroids, int x, int y) {
        List<Double> angles = new ArrayList<>();
        for (int otherY = 0; otherY < asteroids.size(); otherY++) {
            for (int otherX = 0; otherX < asteroids.get(otherY).length; otherX++) {
                if (asteroids.get(otherY)[otherX] && !(otherX == x && otherY == y)) {
                    double angle = Math.atan2(otherY - y, otherX - x);
                    if (!angles.contains(angle)) {
                        angles.add(angle);
                    }
                }
            }
        }
        return angles.size();
    }
}
