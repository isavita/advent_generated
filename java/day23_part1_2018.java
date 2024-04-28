import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {
    static class Nanobot {
        int x, y, z, radius;

        Nanobot(int x, int y, int z, int radius) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.radius = radius;
        }
    }

    public static void main(String[] args) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<Nanobot> nanobots = parseNanobots(br);

            Nanobot strongest = findStrongestNanobot(nanobots);
            int inRangeCount = countNanobotsInRange(nanobots, strongest);

            System.out.println(inRangeCount);
        }
    }

    static List<Nanobot> parseNanobots(BufferedReader br) throws IOException {
        Pattern re = Pattern.compile("pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)");
        List<Nanobot> nanobots = new ArrayList<>();

        String line;
        while ((line = br.readLine()) != null) {
            Matcher m = re.matcher(line);
            if (m.matches()) {
                int x = Integer.parseInt(m.group(1));
                int y = Integer.parseInt(m.group(2));
                int z = Integer.parseInt(m.group(3));
                int radius = Integer.parseInt(m.group(4));

                nanobots.add(new Nanobot(x, y, z, radius));
            }
        }

        return nanobots;
    }

    static Nanobot findStrongestNanobot(List<Nanobot> nanobots) {
        Nanobot strongest = nanobots.get(0);
        for (Nanobot nanobot : nanobots) {
            if (nanobot.radius > strongest.radius) {
                strongest = nanobot;
            }
        }
        return strongest;
    }

    static int countNanobotsInRange(List<Nanobot> nanobots, Nanobot strongest) {
        int count = 0;
        for (Nanobot nanobot : nanobots) {
            if (manhattanDistance(nanobot, strongest) <= strongest.radius) {
                count++;
            }
        }
        return count;
    }

    static int manhattanDistance(Nanobot a, Nanobot b) {
        return Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z);
    }
}