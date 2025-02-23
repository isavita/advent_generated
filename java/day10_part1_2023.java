
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Main {
    static final int[] Top = {0, -1};
    static final int[] Right = {1, 0};
    static final int[] Bottom = {0, 1};
    static final int[] Left = {-1, 0};
    static final char Empty = '.';
    static final char Start = 'S';
    static final char Vertical = '|';
    static final char Horizontal = '-';
    static final char TopLeftCorner = 'J';
    static final char TopRightCorner = 'L';
    static final char BottomLeftCorner = '7';
    static final char BottomRightCorner = 'F';

    static Map<Character, Set<List<Integer>>> TileToPipe = Map.of(
            Vertical, Set.of(Arrays.asList(Top[0], Top[1]), Arrays.asList(Bottom[0], Bottom[1])),
            Horizontal, Set.of(Arrays.asList(Left[0], Left[1]), Arrays.asList(Right[0], Right[1])),
            TopLeftCorner, Set.of(Arrays.asList(Top[0], Top[1]), Arrays.asList(Left[0], Left[1])),
            TopRightCorner, Set.of(Arrays.asList(Top[0], Top[1]), Arrays.asList(Right[0], Right[1])),
            BottomLeftCorner, Set.of(Arrays.asList(Bottom[0], Bottom[1]), Arrays.asList(Left[0], Left[1])),
            BottomRightCorner, Set.of(Arrays.asList(Bottom[0], Bottom[1]), Arrays.asList(Right[0], Right[1]))
    );

    static Map<Character, Set<List<Integer>>> getPipeFromTile() {
        return TileToPipe;
    }

    static Map<List<Integer>, Character> buildReverseMapping(Map<Character, Set<List<Integer>>> tileToPipe) {
        Map<List<Integer>, Character> reverseMap = new HashMap<>();
        for (Map.Entry<Character, Set<List<Integer>>> entry : tileToPipe.entrySet()) {
            for (List<Integer> pipe : entry.getValue()) {
                reverseMap.put(pipe, entry.getKey());
            }
        }
        return reverseMap;
    }

    static Map<List<Integer>, Character> PipeToTile = buildReverseMapping(getPipeFromTile());

    static char getTileFromPipe(Set<List<Integer>> pipe) {
        if (pipe.equals(TileToPipe.get(Vertical))) return Vertical;
        if (pipe.equals(TileToPipe.get(Horizontal))) return Horizontal;
        if (pipe.equals(TileToPipe.get(TopLeftCorner))) return TopLeftCorner;
        if (pipe.equals(TileToPipe.get(TopRightCorner))) return TopRightCorner;
        if (pipe.equals(TileToPipe.get(BottomLeftCorner))) return BottomLeftCorner;
        if (pipe.equals(TileToPipe.get(BottomRightCorner))) return BottomRightCorner;
        return Empty;
    }

    static Map<List<Integer>, Character> buildGrid(List<String> input) {
        Map<List<Integer>, Character> grid = new HashMap<>();
        for (int y = 0; y < input.size(); y++) {
            String line = input.get(y);
            for (int x = 0; x < line.length(); x++) {
                char charAt = line.charAt(x);
                if (charAt != Empty) {
                    grid.put(Arrays.asList(x, y), charAt);
                }
            }
        }
        return grid;
    }

    static List<Integer> findStart(Map<List<Integer>, Character> grid) {
        for (Map.Entry<List<Integer>, Character> entry : grid.entrySet()) {
            if (entry.getValue() == Start) {
                return entry.getKey();
            }
        }
        return Arrays.asList(0, 0);
    }

    static Set<List<Integer>> getPipeFromNeighbors(List<Integer> coord, Map<List<Integer>, Character> grid) {
        Set<List<Integer>> pipe = new HashSet<>();
        Map<List<Integer>, List<Integer>> possibleNeighbors = Map.of(
                Arrays.asList(Top[0], Top[1]), Arrays.asList(coord.get(0) + Top[0], coord.get(1) + Top[1]),
                Arrays.asList(Right[0], Right[1]), Arrays.asList(coord.get(0) + Right[0], coord.get(1) + Right[1]),
                Arrays.asList(Bottom[0], Bottom[1]), Arrays.asList(coord.get(0) + Bottom[0], coord.get(1) + Bottom[1]),
                Arrays.asList(Left[0], Left[1]), Arrays.asList(coord.get(0) + Left[0], coord.get(1) + Left[1])
        );
        for (Map.Entry<List<Integer>, List<Integer>> entry : possibleNeighbors.entrySet()) {
            List<Integer> neighborCoord = entry.getValue();
            if (grid.containsKey(neighborCoord)) {
                Set<List<Integer>> neighborPipe = getPipeFromTile().get(grid.get(neighborCoord));
                if (neighborPipe != null) {
                    int negDirX = -entry.getKey().get(0);
                    int negDirY = -entry.getKey().get(1);
                    if (neighborPipe.contains(Arrays.asList(negDirX, negDirY))) {
                        pipe.add(entry.getKey());
                    }
                }
            }
        }
        return pipe;
    }

    static List<List<Integer>> pathFinding(List<Integer> start, Map<List<Integer>, Character> grid) {
        List<List<Integer>> path = new ArrayList<>();
        path.add(start);
        Set<List<Integer>> startPipe = getPipeFromNeighbors(start, grid);

        List<Integer> previousDir = null;
        List<Integer> current = null;

        for (List<Integer> dir : startPipe) {
            previousDir = dir;
            current = Arrays.asList(start.get(0) + dir.get(0), start.get(1) + dir.get(1));
            break;
        }

        while (!current.equals(start)) {
            path.add(current);
            Set<List<Integer>> currentPipe = getPipeFromTile().get(grid.get(current));
            for (List<Integer> dir : currentPipe) {
                if (!dir.equals(Arrays.asList(-previousDir.get(0), -previousDir.get(1)))) {
                    previousDir = dir;
                    current = Arrays.asList(current.get(0) + dir.get(0), current.get(1) + dir.get(1));
                    break;
                }
            }
        }
        return path;
    }

    static int solve(List<String> input) {
        Map<List<Integer>, Character> grid = buildGrid(input);
        List<Integer> start = findStart(grid);
        List<List<Integer>> path = pathFinding(start, grid);
        return path.size() / 2;
    }

    public static void main(String[] args) throws IOException {
        List<String> input = Files.readAllLines(Paths.get("input.txt"));
        System.out.println(solve(input));
    }
}
