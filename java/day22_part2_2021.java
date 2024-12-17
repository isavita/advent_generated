
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {

    public static void main(String[] args) throws IOException {
        String input = Files.readString(Paths.get("input.txt")).trim();
        long result = solve(input);
        System.out.println(result);
    }

    static long solve(String input) {
        List<Cube> cubes = parseInput(input);
        List<Cube> finalList = new ArrayList<>();

        for (Cube c : cubes) {
            List<Cube> toAdd = new ArrayList<>();
            for (Cube finalCube : finalList) {
                Cube intersection = finalCube.getIntersection(c);
                if (intersection != null) {
                    toAdd.add(intersection);
                }
            }
            if (c.isOn) {
                toAdd.add(c);
            }
            finalList.addAll(toAdd);
        }

        long total = 0;
        for (Cube c : finalList) {
            total += c.volume();
        }
        return total;
    }

    static class Cube {
        boolean isOn;
        int x1, x2, y1, y2, z1, z2;

        Cube(boolean isOn, int x1, int x2, int y1, int y2, int z1, int z2) {
            this.isOn = isOn;
            this.x1 = x1;
            this.x2 = x2;
            this.y1 = y1;
            this.y2 = y2;
            this.z1 = z1;
            this.z2 = z2;
        }

        Cube getIntersection(Cube other) {
            int x1 = Math.max(this.x1, other.x1);
            int x2 = Math.min(this.x2, other.x2);
            int y1 = Math.max(this.y1, other.y1);
            int y2 = Math.min(this.y2, other.y2);
            int z1 = Math.max(this.z1, other.z1);
            int z2 = Math.min(this.z2, other.z2);

            if (x1 > x2 || y1 > y2 || z1 > z2) {
                return null;
            }

            boolean intersectionState;
            if (this.isOn && other.isOn) {
                intersectionState = false;
            } else if (!this.isOn && !other.isOn) {
                intersectionState = true;
            } else {
                intersectionState = other.isOn;
            }

            return new Cube(intersectionState, x1, x2, y1, y2, z1, z2);
        }

        long volume() {
            long vol = (long) (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1);
            return isOn ? vol : -vol;
        }
    }

    static List<Cube> parseInput(String input) {
        List<Cube> cubes = new ArrayList<>();
        Pattern pattern = Pattern.compile("(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)");
        Matcher matcher = pattern.matcher(input);

        while (matcher.find()) {
            boolean isOn = matcher.group(1).equals("on");
            int x1 = Integer.parseInt(matcher.group(2));
            int x2 = Integer.parseInt(matcher.group(3));
            int y1 = Integer.parseInt(matcher.group(4));
            int y2 = Integer.parseInt(matcher.group(5));
            int z1 = Integer.parseInt(matcher.group(6));
            int z2 = Integer.parseInt(matcher.group(7));

            if (x1 > x2 || y1 > y2 || z1 > z2) {
                throw new IllegalArgumentException("Invalid input: coordinates are not in the correct order");
            }

            cubes.add(new Cube(isOn, x1, x2, y1, y2, z1, z2));
        }
        return cubes;
    }
}
