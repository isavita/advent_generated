
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Constellations {

    public static void main(String[] args) {
        try {
            List<Point> points = readPoints("input.txt");
            int numConstellations = countConstellations(points);
            System.out.println(numConstellations);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    static class Point {
        int x, y, z, w;

        public Point(int x, int y, int z, int w) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.w = w;
        }

        public int manhattanDistance(Point other) {
            return Math.abs(this.x - other.x) + Math.abs(this.y - other.y) +
                   Math.abs(this.z - other.z) + Math.abs(this.w - other.w);
        }
    }

    static List<Point> readPoints(String filename) throws IOException {
        List<Point> points = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String[] coords = line.split(",");
                points.add(new Point(Integer.parseInt(coords[0]),
                                     Integer.parseInt(coords[1]),
                                     Integer.parseInt(coords[2]),
                                     Integer.parseInt(coords[3])));
            }
        }
        return points;
    }

    static int countConstellations(List<Point> points) {
        List<List<Point>> constellations = new ArrayList<>();
        for (Point point : points) {
            boolean addedToConstellation = false;
            for (int i = 0; i < constellations.size(); i++) {
                List<Point> constellation = constellations.get(i);
                for (Point existingPoint : constellation) {
                    if (point.manhattanDistance(existingPoint) <= 3) {
                        constellation.add(point);
                        addedToConstellation = true;
                        
                        // Merge constellations if necessary
                        for (int j = i + 1; j < constellations.size(); j++) {
                            List<Point> otherConstellation = constellations.get(j);
                            for (Point otherPoint : otherConstellation) {
                                if (point.manhattanDistance(otherPoint) <= 3) {
                                    constellation.addAll(otherConstellation);
                                    constellations.remove(j);
                                    j--; 
                                    break;
                                }
                            }
                        }
                        break;
                    }
                }
                if (addedToConstellation) break;
            }
            if (!addedToConstellation) {
                List<Point> newConstellation = new ArrayList<>();
                newConstellation.add(point);
                constellations.add(newConstellation);
            }
        }
        return constellations.size();
    }
}
