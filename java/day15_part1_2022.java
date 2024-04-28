import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Main {
    static class Sensor {
        int x, y, bx, by, dist;

        public Sensor(int x, int y, int bx, int by) {
            this.x = x;
            this.y = y;
            this.bx = bx;
            this.by = by;
            this.dist = Math.abs(x - bx) + Math.abs(y - by);
        }
    }

    public static void main(String[] args) throws IOException {
        List<Sensor> sensors = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            for (String line; (line = br.readLine()) != null;) {
                String[] parts = line.split(": ");
                String[] sensorParts = parts[0].split(", ");
                int sx = Integer.parseInt(sensorParts[0].split("x=")[1]);
                int sy = Integer.parseInt(sensorParts[1].split("y=")[1]);
                String[] beaconParts = parts[1].split(", ");
                int bx = Integer.parseInt(beaconParts[0].split("x=")[1]);
                int by = Integer.parseInt(beaconParts[1].split("y=")[1]);
                sensors.add(new Sensor(sx, sy, bx, by));
            }
        }
        System.out.println(impossible(sensors, 2000000));
    }

    public static int impossible(List<Sensor> sensors, int y) {
        Set<Integer> pts = new HashSet<>();
        for (Sensor s : sensors) {
            int dist = s.dist - Math.abs(s.y - y);
            for (int x = 0; x <= dist; x++) {
                pts.add(s.x + x);
                pts.add(s.x - x);
            }
        }
        for (Sensor s : sensors) {
            if (s.by == y) {
                pts.remove(s.bx);
            }
        }
        return pts.size();
    }
}