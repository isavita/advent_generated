
import java.awt.geom.Point2D;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Main {

    private static final Point2D.Double UP = new Point2D.Double(0, -1);
    private static final Point2D.Double DOWN = new Point2D.Double(0, 1);
    private static final Point2D.Double LEFT = new Point2D.Double(-1, 0);
    private static final Point2D.Double RIGHT = new Point2D.Double(1, 0);

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Paths.get("input.txt"));
        System.out.println((int) solve(input));
        System.out.println((int) solve(scaleUp(input)));
    }

    private static double solve(String input) {
        Map<Point2D.Double, Character> m = new HashMap<>();
        List<Point2D.Double> steps = parse(input, m);
        Point2D.Double robot = null;
        for (Map.Entry<Point2D.Double, Character> entry : m.entrySet()) {
            if (entry.getValue() == '@') {
                robot = entry.getKey();
                break;
            }
        }
        for (Point2D.Double dir : steps) {
            if (tryToStep(m, robot, dir)) {
                robot = new Point2D.Double(robot.getX() + dir.getX(), robot.getY() + dir.getY());
            }
        }
        double sum = 0;
        for (Map.Entry<Point2D.Double, Character> entry : m.entrySet()) {
            if (entry.getValue() == '[' || entry.getValue() == 'O') {
                sum += entry.getKey().getX() + 100 * entry.getKey().getY();
            }
        }
        return sum;
    }

    private static boolean tryToStep(Map<Point2D.Double, Character> m, Point2D.Double pos, Point2D.Double dir) {
        Map<Point2D.Double, Character> orig = copyMap(m);
        char current = m.getOrDefault(pos, ' ');
        if (current == '.') {
            return true;
        } else if (current == 'O' || current == '@') {
            Point2D.Double nextPos = new Point2D.Double(pos.getX() + dir.getX(), pos.getY() + dir.getY());
            if (tryToStep(m, nextPos, dir)) {
                m.put(nextPos, current);
                m.put(pos, '.');
                return true;
            }
        } else if (current == ']') {
            Point2D.Double nextPos = new Point2D.Double(pos.getX() + LEFT.getX(), pos.getY() + LEFT.getY());
            if (tryToStep(m, nextPos, dir)) {
                return true;
            }
        } else if (current == '[') {
            if (dir.equals(LEFT)) {
                Point2D.Double nextPos = new Point2D.Double(pos.getX() + LEFT.getX(), pos.getY() + LEFT.getY());
                if (tryToStep(m, nextPos, dir)) {
                    m.put(nextPos, '[');
                    m.put(pos, ']');
                    m.put(new Point2D.Double(pos.getX() + RIGHT.getX(), pos.getY() + RIGHT.getY()), '.');
                    return true;
                }
            } else if (dir.equals(RIGHT)) {
                Point2D.Double nextPos = new Point2D.Double(pos.getX() + 2 * RIGHT.getX(), pos.getY() + 2 * RIGHT.getY());
                if (tryToStep(m, nextPos, dir)) {
                    m.put(pos, '.');
                    m.put(new Point2D.Double(pos.getX() + RIGHT.getX(), pos.getY() + RIGHT.getY()), '[');
                    m.put(nextPos, ']');
                    return true;
                }
            } else {
                Point2D.Double nextPos = new Point2D.Double(pos.getX() + dir.getX(), pos.getY() + dir.getY());
                Point2D.Double nextPosRight = new Point2D.Double(pos.getX() + RIGHT.getX() + dir.getX(), pos.getY() + RIGHT.getY() + dir.getY());
                if (tryToStep(m, nextPos, dir) && tryToStep(m, nextPosRight, dir)) {
                    m.put(pos, '.');
                    m.put(new Point2D.Double(pos.getX() + RIGHT.getX(), pos.getY() + RIGHT.getY()), '.');
                    m.put(nextPos, '[');
                    m.put(nextPosRight, ']');
                    return true;
                }
            }
        }
        m.clear();
        m.putAll(orig);
        return false;
    }

    private static String scaleUp(String input) {
        String s = input;
        s = s.replace("#", "##");
        s = s.replace(".", "..");
        s = s.replace("O", "[]");
        s = s.replace("@", "@.");
        return s;
    }

    private static List<Point2D.Double> parse(String input, Map<Point2D.Double, Character> m) {
        String[] blocks = input.trim().split("\n\n");
        String[] lines = blocks[0].split("\n");
        for (int y = 0; y < lines.length; y++) {
            for (int x = 0; x < lines[y].length(); x++) {
                m.put(new Point2D.Double(x, y), lines[y].charAt(x));
            }
        }
        List<Point2D.Double> steps = new ArrayList<>();
        for (char ch : blocks[1].replace("\n", "").toCharArray()) {
            switch (ch) {
                case '^':
                    steps.add(UP);
                    break;
                case '<':
                    steps.add(LEFT);
                    break;
                case '>':
                    steps.add(RIGHT);
                    break;
                case 'v':
                    steps.add(DOWN);
                    break;
            }
        }
        return steps;
    }

    private static Map<Point2D.Double, Character> copyMap(Map<Point2D.Double, Character> src) {
        Map<Point2D.Double, Character> dst = new HashMap<>(src);
        return dst;
    }
}
