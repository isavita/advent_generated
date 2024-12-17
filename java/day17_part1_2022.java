
import java.awt.Point;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Main {

    static final String rockstr = "####\n\n # \n###\n # \n\n  #\n  #\n###\n\n#\n#\n#\n#\n\n##\n##";
    static final Point S = new Point(0, -1);

    public static void main(String[] args) throws IOException {
        byte[] jetPattern = Files.readString(Paths.get("input.txt")).trim().getBytes();
        List<Set<Point>> rocks = getRocks();
        Set<Point> grid = new HashSet<>();
        for (int x = 0; x < 7; x++) {
            grid.add(new Point(x, 0));
        }
        int floor = 0;
        int j = 0;
        Map<List<Integer>, List<Integer>> repeat = new HashMap<>();

        for (int i = 0, curr = 0; ; i++, curr = (curr + 1) % rocks.size()) {
            if (i == 2022) {
                System.out.println(floor);
                break;
            }
            List<Integer> key = List.of(curr, j);
            if (repeat.containsKey(key)) {
                List<Integer> prev = repeat.get(key);
                int prevI = prev.get(0);
                int prevFloor = prev.get(1);
                int cycleLen = i - prevI;
                int cycleHeight = floor - prevFloor;
                int remaining = 2022 - i;
                int cycles = remaining / cycleLen;
                floor += cycles * cycleHeight;
                i += cycles * cycleLen;
                if (i >= 2022) {
                    System.out.println(floor);
                    break;
                }
            }
            repeat.put(key, List.of(i, floor));
            Set<Point> currRock = rocks.get(curr);
            Point pos = new Point(2, floor + 4);
            while (true) {
                byte jet = jetPattern[j];
                j = (j + 1) % jetPattern.length;
                pos.translate(dirFromByte(jet).x, dirFromByte(jet).y);
                if (collision(grid, currRock, pos)) {
                    pos.translate(-dirFromByte(jet).x, -dirFromByte(jet).y);
                }
                pos.translate(S.x, S.y);
                if (collision(grid, currRock, pos)) {
                    pos.translate(-S.x, -S.y);
                    for (Point p : currRock) {
                        Point newP = new Point(p.x + pos.x, p.y + pos.y);
                        grid.add(newP);
                        floor = Math.max(floor, newP.y);
                    }
                    break;
                }
            }
        }
    }

    static boolean collision(Set<Point> grid, Set<Point> rock, Point pos) {
        for (Point p : rock) {
            Point newP = new Point(p.x + pos.x, p.y + pos.y);
            if (grid.contains(newP) || newP.x < 0 || newP.x > 6) {
                return true;
            }
        }
        return false;
    }

    static List<Set<Point>> getRocks() {
        List<Set<Point>> rocks = new ArrayList<>();
        String[] rockStrings = rockstr.split("\n\n");
        for (String rock : rockStrings) {
            Set<Point> currentRock = new HashSet<>();
            String[] lines = rock.split("\n");
            for (int y = 0; y < lines.length; y++) {
                String line = lines[y];
                for (int x = 0; x < line.length(); x++) {
                    if (line.charAt(x) == '#') {
                        currentRock.add(new Point(x, lines.length - 1 - y));
                    }
                }
            }
            rocks.add(currentRock);
        }
        return rocks;
    }

    static Point dirFromByte(byte b) {
        return switch (b) {
            case '>', 'E', 'R' -> new Point(1, 0);
            case '<', 'W', 'L' -> new Point(-1, 0);
            case '^', 'N', 'U' -> new Point(0, 1);
            case 'v', 'S', 'D' -> new Point(0, -1);
            default -> new Point(0, 0);
        };
    }
}
