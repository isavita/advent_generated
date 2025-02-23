
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Solver {

    static final Coord Top = new Coord(0, -1);
    static final Coord Right = new Coord(1, 0);
    static final Coord Bottom = new Coord(0, 1);
    static final Coord Left = new Coord(-1, 0);

    static final char Empty = '.';
    static final char Vertical = '|';
    static final char Horizontal = '-';
    static final char TopLeftCorner = 'J';
    static final char TopRightCorner = 'L';
    static final char BottomLeftCorner = '7';
    static final char BottomRightCorner = 'F';

    record Coord(int x, int y) {
        Coord add(Coord other) {
            return new Coord(this.x + other.x, this.y + other.y);
        }

        Coord opposite() {
            return new Coord(-this.x, -this.y);
        }
    }

    static Map<Character, Set<Coord>> TileToPipe = Map.of(
            Vertical, Set.of(Top, Bottom),
            Horizontal, Set.of(Left, Right),
            TopLeftCorner, Set.of(Top, Left),
            TopRightCorner, Set.of(Top, Right),
            BottomLeftCorner, Set.of(Bottom, Left),
            BottomRightCorner, Set.of(Bottom, Right)
    );

    static Map<Coord, Character> buildGrid(List<String> lines) {
        Map<Coord, Character> grid = new HashMap<>();
        for (int y = 0; y < lines.size(); y++) {
            String line = lines.get(y);
            for (int x = 0; x < line.length(); x++) {
                char c = line.charAt(x);
                if (c != Empty) {
                    grid.put(new Coord(x, y), c);
                }
            }
        }
        return grid;
    }

    static Coord findStart(Map<Coord, Character> grid) {
        for (Map.Entry<Coord, Character> entry : grid.entrySet()) {
            if (entry.getValue() == 'S') {
                return entry.getKey();
            }
        }
        return new Coord(0, 0);
    }

    static Set<Coord> getPipeFromNeighbors(Coord coord, Map<Coord, Character> grid) {
        Set<Coord> pipe = new HashSet<>();
        Coord[] possibleNeighbors = {Top, Right, Bottom, Left};

        for (Coord dir : possibleNeighbors) {
            Coord neighborCoord = coord.add(dir);
            if (grid.containsKey(neighborCoord)) {
                Set<Coord> neighborPipe = TileToPipe.get(grid.get(neighborCoord));
                if (neighborPipe != null && neighborPipe.contains(dir.opposite())) {
                    pipe.add(dir);
                }
            }
        }
        return pipe;
    }

    static List<Coord> pathFinding(Coord start, Map<Coord, Character> grid) {
        List<Coord> path = new ArrayList<>();
        path.add(start);
        Set<Coord> startPipe = getPipeFromNeighbors(start, grid);
        Coord previousDir = null;
        Coord current = null;

        for (Coord dir : startPipe) {
            previousDir = dir;
            current = start.add(dir);
            break;
        }

        while (!current.equals(start)) {
            path.add(current);
            Set<Coord> currentPipe = TileToPipe.get(grid.get(current));
            for (Coord dir : currentPipe) {
                if (!dir.equals(previousDir.opposite())) {
                    previousDir = dir;
                    current = current.add(dir);
                    break;
                }
            }
        }
        return path;
    }

    static char getTileFromPipe(Set<Coord> pipe) {
        for (Map.Entry<Character, Set<Coord>> entry : TileToPipe.entrySet()) {
            if (entry.getValue().equals(pipe)) {
                return entry.getKey();
            }
        }
        return Empty;
    }

    static Map<Coord, Character> getPathGrid(Map<Coord, Character> grid, List<Coord> path) {
        Map<Coord, Character> newGrid = new HashMap<>();
        for (Coord coord : path) {
            newGrid.put(coord, grid.get(coord));
        }
        Coord start = path.get(0);
        newGrid.put(start, getTileFromPipe(getPipeFromNeighbors(start, grid)));
        return newGrid;
    }

    static boolean isInside(Coord coord, Map<Coord, Character> grid, int maxX) {
        if (grid.containsKey(coord)) {
            return false;
        }

        int numPipeOnLeft = 0;
        char startPipe = Empty;

        for (int x = 0; x < coord.x; x++) {
            Coord c = new Coord(x, coord.y);
            if (grid.containsKey(c)) {
                char v = grid.get(c);
                if (v == Vertical) {
                    numPipeOnLeft++;
                } else if (v == TopRightCorner) {
                    startPipe = TopRightCorner;
                } else if (v == BottomRightCorner) {
                    startPipe = BottomRightCorner;
                } else if (v == TopLeftCorner) {
                    if (startPipe == BottomRightCorner) {
                        numPipeOnLeft++;
                    }
                    startPipe = Empty;
                } else if (v == BottomLeftCorner) {
                    if (startPipe == TopRightCorner) {
                        numPipeOnLeft++;
                    }
                    startPipe = Empty;
                }
            }
        }
        return numPipeOnLeft % 2 == 1;
    }

    static int solve(List<String> lines) {
        Map<Coord, Character> grid = buildGrid(lines);
        Coord start = findStart(grid);
        List<Coord> path = pathFinding(start, grid);
        Map<Coord, Character> pathGrid = getPathGrid(grid, path);

        int count = 0;
        int maxY = lines.size();
        int maxX = lines.get(0).length();

        for (int y = 0; y < maxY; y++) {
            for (int x = 0; x < maxX; x++) {
                Coord c = new Coord(x, y);
                if (isInside(c, pathGrid, maxX)) {
                    count++;
                }
            }
        }
        return count;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines = Files.readAllLines(Paths.get("input.txt"));
        System.out.println(solve(lines));
    }
}
