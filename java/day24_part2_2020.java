
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

    static List<Coordinate> getNeighbors(Coordinate tile) {
        List<Coordinate> neighbors = new ArrayList<>();
        for (Coordinate dir : directions.values()) {
            neighbors.add(new Coordinate(tile.q + dir.q, tile.r + dir.r));
        }
        return neighbors;
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
        Set<Coordinate> blackTiles = new HashSet<>();
        String line;

        while ((line = reader.readLine()) != null) {
            Coordinate coord = new Coordinate(0, 0);
            for (int i = 0; i < line.length(); i++) {
                String dir;
                if (line.charAt(i) == 'e' || line.charAt(i) == 'w') {
                    dir = String.valueOf(line.charAt(i));
                } else {
                    dir = line.substring(i, i + 2);
                    i++;
                }
                Coordinate move = directions.get(dir);
                coord.q += move.q;
                coord.r += move.r;
            }
            if (!blackTiles.add(coord)) blackTiles.remove(coord);
        }
        reader.close();

        for (int day = 0; day < 100; day++) {
            Set<Coordinate> tilesToCheck = new HashSet<>();
            for (Coordinate tile : blackTiles) {
                tilesToCheck.add(tile);
                tilesToCheck.addAll(getNeighbors(tile));
            }

            Set<Coordinate> newBlackTiles = new HashSet<>();
            for (Coordinate tile : tilesToCheck) {
                long blackNeighborCount = getNeighbors(tile).stream().filter(blackTiles::contains).count();
                if (blackTiles.contains(tile) && (blackNeighborCount == 1 || blackNeighborCount == 2)) {
                    newBlackTiles.add(tile);
                } else if (!blackTiles.contains(tile) && blackNeighborCount == 2) {
                    newBlackTiles.add(tile);
                }
            }
            blackTiles = newBlackTiles;
        }

        System.out.println(blackTiles.size());
    }
}
