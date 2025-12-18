
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Cafeteria {

    /**
     * Represents a range of ingredient IDs.
     */
    static class Range implements Comparable<Range> {
        long start;
        long end;

        Range(long start, long end) {
            this.start = start;
            this.end = end;
        }

        @Override
        public int compareTo(Range other) {
            return Long.compare(this.start, other.start);
        }
    }

    public static void main(String[] args) {
        String fileName = "input.txt";
        List<String> lines;

        try {
            lines = Files.readAllLines(Paths.get(fileName));
        } catch (IOException e) {
            System.err.println("Error: Could not read " + fileName);
            return;
        }

        List<Range> rawRanges = new ArrayList<>();
        List<Long> availableIds = new ArrayList<>();
        boolean parsingRanges = true;

        // 1. Parse Input
        for (String line : lines) {
            line = line.trim();
            if (line.isEmpty()) {
                if (!rawRanges.isEmpty()) parsingRanges = false;
                continue;
            }

            if (parsingRanges && line.contains("-")) {
                String[] parts = line.split("-");
                rawRanges.add(new Range(Long.parseLong(parts[0]), Long.parseLong(parts[1])));
            } else {
                parsingRanges = false;
                availableIds.add(Long.parseLong(line));
            }
        }

        if (rawRanges.isEmpty()) return;

        // 2. Optimize: Sort and Merge Overlapping Ranges
        // This makes both Part 1 and Part 2 calculation highly efficient.
        Collections.sort(rawRanges);
        List<Range> mergedRanges = new ArrayList<>();
        Range current = new Range(rawRanges.get(0).start, rawRanges.get(0).end);

        for (int i = 1; i < rawRanges.size(); i++) {
            Range next = rawRanges.get(i);
            if (next.start <= current.end) {
                current.end = Math.max(current.end, next.end);
            } else {
                mergedRanges.add(current);
                current = new Range(next.start, next.end);
            }
        }
        mergedRanges.add(current);

        // 3. Solve Part 1: Count available IDs that fall into any fresh range
        long freshAvailableCount = 0;
        for (long id : availableIds) {
            for (Range r : mergedRanges) {
                if (id >= r.start && id <= r.end) {
                    freshAvailableCount++;
                    break;
                }
            }
        }

        // 4. Solve Part 2: Total number of unique IDs considered fresh
        long totalFreshIds = 0;
        for (Range r : mergedRanges) {
            totalFreshIds += (r.end - r.start + 1);
        }

        // Output results
        System.out.println("Part One: " + freshAvailableCount);
        System.out.println("Part Two: " + totalFreshIds);
    }
}
