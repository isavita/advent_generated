
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Firewall {

    public static void main(String[] args) {
        List<Range> blockedRanges = readBlockedRanges("input.txt");
        Collections.sort(blockedRanges, (r1, r2) -> Long.compare(r1.start, r2.start));

        long lowestAllowed = findLowestAllowed(blockedRanges);
        System.out.println(lowestAllowed);

        long allowedCount = countAllowed(blockedRanges);
        System.out.println(allowedCount);
    }

    private static List<Range> readBlockedRanges(String filename) {
        List<Range> ranges = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split("-");
                long start = Long.parseLong(parts[0]);
                long end = Long.parseLong(parts[1]);
                ranges.add(new Range(start, end));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return ranges;
    }

    private static long findLowestAllowed(List<Range> blockedRanges) {
        long current = 0;
        for (Range range : blockedRanges) {
            if (current < range.start) {
                return current;
            }
            current = Math.max(current, range.end + 1);
        }
        return current;
    }

    private static long countAllowed(List<Range> blockedRanges) {
        long allowedCount = 0;
        long current = 0;
        for (Range range : blockedRanges) {
            if (current < range.start) {
                allowedCount += (range.start - current);
            }
            current = Math.max(current, range.end + 1);
        }
        allowedCount += (4294967295L - current + 1);
        return allowedCount;
    }

    static class Range {
        long start;
        long end;

        public Range(long start, long end) {
            this.start = start;
            this.end = end;
        }
    }
}
