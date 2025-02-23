
import java.io.*;
import java.util.*;

public class Solution {
    static final long OFFSET = 10000000000000L;

    public static void main(String[] args) throws IOException {
        List<long[]> machines = readInput("input.txt");
        List<Long> results = new ArrayList<>();
        for (long[] machine : machines) {
            long cost = solveMachine(machine[0], machine[1], machine[2], machine[3], machine[4] + OFFSET, machine[5] + OFFSET);
            if (cost >= 0) {
                results.add(cost);
            }
        }
        if (results.isEmpty()) {
            System.out.println("0 0");
            return;
        }
        long sum = 0;
        for (long result : results) {
            sum += result;
        }
        System.out.println(results.size() + " " + sum);
    }

    static List<long[]> readInput(String filename) throws IOException {
        List<long[]> machines = new ArrayList<>();
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.strip();
                if (line.isEmpty()) {
                    if (!lines.isEmpty()) {
                        machines.add(parseMachine(lines));
                        lines.clear();
                    }
                } else {
                    lines.add(line);
                }
            }
            if (!lines.isEmpty()) {
                machines.add(parseMachine(lines));
            }
        }
        return machines;
    }

    static long[] parseMachine(List<String> lines) {
        long ax = 0, ay = 0, bx = 0, by = 0, px = 0, py = 0;
        for (String line : lines) {
            line = line.replace("Button A:", "A:").replace("Button B:", "B:").replace("Prize:", "P:");
            if (line.startsWith("A:")) {
                long[] parsed = parseLine(line.substring(2));
                ax = parsed[0];
                ay = parsed[1];
            } else if (line.startsWith("B:")) {
                long[] parsed = parseLine(line.substring(2));
                bx = parsed[0];
                by = parsed[1];
            } else if (line.startsWith("P:")) {
                long[] parsed = parsePrize(line.substring(2));
                px = parsed[0];
                py = parsed[1];
            }
        }
        return new long[]{ax, ay, bx, by, px, py};
    }

    static long[] parseLine(String s) {
        String[] parts = s.strip().split(",");
        return new long[]{
                Long.parseLong(parts[0].replaceAll("[XY+=]", "").strip()),
                Long.parseLong(parts[1].replaceAll("[XY+=]", "").strip())
        };
    }

    static long[] parsePrize(String s) {
        String[] parts = s.strip().split(",");
        return new long[]{
                Long.parseLong(parts[0].replace("X=", "").strip()),
                Long.parseLong(parts[1].replace("Y=", "").strip())
        };
    }

    static long solveMachine(long ax, long ay, long bx, long by, long px, long py) {
        long D = ax * by - ay * bx;
        if (D == 0) {
            return -1;
        }
        long numA = px * by - py * bx;
        long numB = -px * ay + py * ax;
        if (numA % D != 0 || numB % D != 0) {
            return -1;
        }
        long a = numA / D;
        long b = numB / D;
        if (a < 0 || b < 0) {
            return -1;
        }
        return 3 * a + b;
    }
}
