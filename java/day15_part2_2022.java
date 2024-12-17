
import java.awt.Point;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {

    static class Sensor {
        Point pos;
        Point beacon;
        int dist;
    }

    public static void main(String[] args) {
        List<Sensor> sensors = new ArrayList<>();
        String input = readAll("input.txt");
        Pattern pattern = Pattern.compile("Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)");
        for (String line : input.split("\n")) {
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) {
                Sensor s = new Sensor();
                s.pos = new Point(Integer.parseInt(matcher.group(1)), Integer.parseInt(matcher.group(2)));
                s.beacon = new Point(Integer.parseInt(matcher.group(3)), Integer.parseInt(matcher.group(4)));
                s.dist = manhattan(s.pos, s.beacon);
                sensors.add(s);
            }
        }
        System.out.println(distress(sensors, 4000000));
    }

    static long distress(List<Sensor> sensors, int maxcoord) {
        for (int x = 0; x <= maxcoord; x++) {
            int y = 0;
            while (y <= maxcoord) {
                Point p = new Point(x, y);
                boolean detected = false;
                int skip = 0;
                for (Sensor s : sensors) {
                    if (manhattan(s.pos, p) <= s.dist) {
                        detected = true;
                        int dist = s.dist - Math.abs(s.pos.x - x);
                        skip = Math.max(skip, dist + s.pos.y - y);
                    }
                }
                if (!detected) {
                    return (long) x * 4000000 + y;
                }
                y += Math.max(1,skip);
            }
        }
        return -1;
    }

    static String readAll(String path) {
        try {
            return new String(Files.readAllBytes(Paths.get(path)));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    static int abs(int n) {
        return n < 0 ? -n : n;
    }

    static int manhattan(Point p, Point q) {
        return abs(p.x - q.x) + abs(p.y - q.y);
    }
}
