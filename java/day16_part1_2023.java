
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;

public class Solution {
    static class Coord {
        int x, y;

        Coord(int x, int y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (obj == null || getClass() != obj.getClass()) return false;
            Coord coord = (Coord) obj;
            return x == coord.x && y == coord.y;
        }

        @Override
        public int hashCode() {
            return 31 * x + y;
        }

        Coord add(Coord other) {
            return new Coord(this.x + other.x, this.y + other.y);
        }

        Coord rotate90() {
            return new Coord(this.y, -this.x);
        }

        Coord rotateNeg90() {
            return new Coord(-this.y, this.x);
        }

        boolean isInBounds(int width, int height) {
            return 0 <= this.x && this.x < width && 0 <= this.y && this.y < height;
        }
    }

    static class Beam {
        Coord origin;
        Coord dir;

        Beam(Coord origin, Coord dir) {
            this.origin = origin;
            this.dir = dir;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (obj == null || getClass() != obj.getClass()) return false;
            Beam beam = (Beam) obj;
            return origin.equals(beam.origin) && dir.equals(beam.dir);
        }

        @Override
        public int hashCode() {
            return 31 * origin.hashCode() + dir.hashCode();
        }
    }

    static final char EMPTY = '.';
    static final char ASCENDING_MIRROR = '/';
    static final char DESCENDING_MIRROR = '\\';
    static final char VERTICAL_SPLITTER = '|';
    static final char HORIZONTAL_SPLITTER = '-';
    static final Coord NORTH = new Coord(0, -1);
    static final Coord WEST = new Coord(-1, 0);
    static final Coord SOUTH = new Coord(0, 1);
    static final Coord EAST = new Coord(1, 0);

    static char[][] buildGrid(List<String> lines, int[] width, int[] height) {
        height[0] = lines.size();
        width[0] = lines.get(0).length();
        char[][] grid = new char[height[0]][width[0]];
        for (int y = 0; y < height[0]; y++) {
            for (int x = 0; x < width[0]; x++) {
                grid[y][x] = lines.get(y).charAt(x);
            }
        }
        return grid;
    }

    static Queue<Beam> nextBeam(char[][] grid, Beam beam, int width, int height) {
        Queue<Beam> beams = new LinkedList<>();
        char cell = beam.origin.y >= 0 && beam.origin.y < height && beam.origin.x >=0 && beam.origin.x < width? grid[beam.origin.y][beam.origin.x] : EMPTY;

        if (cell == ASCENDING_MIRROR) {
            Coord newDir = (beam.dir.equals(NORTH) || beam.dir.equals(SOUTH)) ? beam.dir.rotateNeg90() : beam.dir.rotate90();
            beams.add(new Beam(beam.origin.add(newDir), newDir));
        } else if (cell == DESCENDING_MIRROR) {
            Coord newDir = (beam.dir.equals(NORTH) || beam.dir.equals(SOUTH)) ? beam.dir.rotate90() : beam.dir.rotateNeg90();
            beams.add(new Beam(beam.origin.add(newDir), newDir));
        } else if (cell == VERTICAL_SPLITTER && (beam.dir.equals(EAST) || beam.dir.equals(WEST))) {
            beams.add(new Beam(beam.origin.add(beam.dir.rotate90()), beam.dir.rotate90()));
            beams.add(new Beam(beam.origin.add(beam.dir.rotateNeg90()), beam.dir.rotateNeg90()));
        } else if (cell == HORIZONTAL_SPLITTER && (beam.dir.equals(NORTH) || beam.dir.equals(SOUTH))) {
            beams.add(new Beam(beam.origin.add(beam.dir.rotate90()), beam.dir.rotate90()));
            beams.add(new Beam(beam.origin.add(beam.dir.rotateNeg90()), beam.dir.rotateNeg90()));
        } else {
            beams.add(new Beam(beam.origin.add(beam.dir), beam.dir));
        }

        return beams;
    }

    static Set<Coord> calculateEnergization(char[][] grid, Beam start, int width, int height) {
        Set<Beam> alreadySeen = new HashSet<>();
        Queue<Beam> toExplore = new LinkedList<>();
        toExplore.add(start);
        Set<Coord> energized = new HashSet<>();

        while (!toExplore.isEmpty()) {
            Beam beam = toExplore.poll();

            if (beam.origin.isInBounds(width, height) && alreadySeen.add(beam)) {
                energized.add(beam.origin);
                toExplore.addAll(nextBeam(grid, beam, width, height));
            }
        }
        return energized;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines = Files.readAllLines(Paths.get("input.txt"));
        int[] width = new int[1];
        int[] height = new int[1];
        char[][] grid = buildGrid(lines, width, height);
        Beam start = new Beam(new Coord(0, 0), EAST);
        Set<Coord> energized = calculateEnergization(grid, start, width[0], height[0]);
        System.out.println(energized.size());
    }
}
