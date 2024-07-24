
import java.io.*;
import java.util.*;

public class Main {
    static class Coordinate {
        int q, r;

        Coordinate(int q, int r) {
            this.q = q;
            this.r = r;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (!(obj instanceof Coordinate)) return false;
            Coordinate other = (Coordinate) obj;
            return q == other.q && r == other.r;
        }

        @Override
        public int hashCode() {
            return Objects.hash(q, r);
        }
    }

    static final Map<String, Coordinate> directions = Map.of(
        "e", new Coordinate(1, 0),
        "se", new Coordinate(0, 1),
        "sw", new Coordinate(-1, 1),
        "w", new Coordinate(-1, 0),
        "nw", new Coordinate(0, -1),
        "ne", new Coordinate(1, -1)
    );

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
        Set<Coordinate> blackTiles = new HashSet<>();
        String line;

        while ((line = reader.readLine()) != null) {
            Coordinate coord = new Coordinate(0, 0);
            for (int i = 0; i < line.length(); ) {
                String dir = String.valueOf(line.charAt(i));
                if (dir.equals("n") || dir.equals("s")) {
                    dir += line.charAt(++i);
                }
                coord.q += directions.get(dir).q;
                coord.r += directions.get(dir).r;
                i++;
            }
            if (!blackTiles.add(coord)) {
                blackTiles.remove(coord);
            }
        }
        System.out.println(blackTiles.size());
    }
}
