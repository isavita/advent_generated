
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

    private static final String ROCKSTR = "####\n\n # \n###\n # \n\n  #\n  #\n###\n\n#\n#\n#\n#\n\n##\n##";
    private static final Point N = new Point(0, 1);
    private static final Point E = new Point(1, 0);
    private static final Point S = new Point(0, -1);
    private static final Point W = new Point(-1, 0);

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
            List<Integer> key = List.of(curr, j);
            if (repeat.containsKey(key)) {
                List<Integer> r = repeat.get(key);
                int previ = r.get(0);
                int prevFloor = r.get(1);
                if ((1000000000000L - i) % (i - previ) == 0) {
                    System.out.println(floor + (1000000000000L - i) / (i - previ) * (floor - prevFloor));
                    break;
                }
            }
            repeat.put(key, List.of(i, floor));
            Set<Point> currRock = rocks.get(curr);
            Point pos = new Point(2, floor + 4);
            while (true) {
                Point jetDir = dirFromByte(jetPattern[j]);
                j = (j + 1) % jetPattern.length;
                pos.translate(jetDir.x, jetDir.y);
                if (collision(grid, currRock, pos)) {
                    pos.translate(-jetDir.x, -jetDir.y);
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

    private static boolean collision(Set<Point> grid, Set<Point> rock, Point pos) {
        for (Point p : rock) {
            Point newP = new Point(p.x + pos.x, p.y + pos.y);
            if (grid.contains(newP) || newP.x < 0 || newP.x > 6) {
                return true;
            }
        }
        return false;
    }

    private static List<Set<Point>> getRocks() {
        List<Set<Point>> rocks = new ArrayList<>();
        String[] rockStrings = ROCKSTR.split("\n\n");
        for (String rockStr : rockStrings) {
            Set<Point> rock = new HashSet<>();
            String[] lines = rockStr.split("\n");
            for (int y = 0; y < lines.length; y++) {
                String line = lines[y];
                for (int x = 0; x < line.length(); x++) {
                    if (line.charAt(x) == '#') {
                        rock.add(new Point(x, lines.length - 1 - y));
                    }
                }
            }
            rocks.add(rock);
        }
        return rocks;
    }

    private static Point dirFromByte(byte b) {
        return switch (b) {
            case 'N', 'U', '^' -> N;
            case 'E', 'R', '>' -> E;
            case 'S', 'D', 'v' -> S;
            case 'W', 'L', '<' -> W;
            default -> throw new IllegalArgumentException("Invalid direction byte: " + (char) b);
        };
    }
}
