
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Scanner;

public class Cafeteria {

    static class Range {
        long start, end;

        Range(long start, long end) {
            this.start = start;
            this.end = end;
        }
    }

    public static void main(String[] args) {
        List<Range> ranges = new ArrayList<>();
        List<Long> ids = new ArrayList<>();

        try (Scanner scanner = new Scanner(new File("input.txt"))) {
            // 1. Read ranges until a blank line is encountered
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine().trim();
                if (line.isEmpty()) break;
                String[] parts = line.split("-");
                ranges.add(new Range(Long.parseLong(parts[0]), Long.parseLong(parts[1])));
            }

            // 2. Read available ingredient IDs
            while (scanner.hasNextLong()) {
                ids.add(scanner.nextLong());
            }
        } catch (FileNotFoundException e) {
            System.err.println("File not found: input.txt");
            return;
        }

        // 3. Optimize ranges: Sort and Merge overlapping intervals
        List<Range> mergedRanges = mergeRanges(ranges);

        // 4. Count IDs that fall within any merged range
        int freshCount = 0;
        for (long id : ids) {
            if (isFresh(id, mergedRanges)) {
                freshCount++;
            }
        }

        System.out.println(freshCount);
    }

    /**
     * Merges overlapping or adjacent ranges to minimize the number of checks needed.
     */
    private static List<Range> mergeRanges(List<Range> ranges) {
        if (ranges.isEmpty()) return ranges;

        // Sort ranges by start ID
        ranges.sort(Comparator.comparingLong(r -> r.start));

        List<Range> merged = new ArrayList<>();
        Range current = ranges.get(0);

        for (int i = 1; i < ranges.size(); i++) {
            Range next = ranges.get(i);
            if (next.start <= current.end + 1) { // +1 handles adjacent ranges like 3-5 and 6-8
                current.end = Math.max(current.end, next.end);
            } else {
                merged.add(current);
                current = next;
            }
        }
        merged.add(current);
        return merged;
    }

    /**
     * Checks if an ID falls within any of the merged ranges.
     * Uses simple iteration; could be upgraded to binary search if range list is massive.
     */
    private static boolean isFresh(long id, List<Range> mergedRanges) {
        for (Range r : mergedRanges) {
            if (id >= r.start && id <= r.end) {
                return true;
            }
            // Since ranges are sorted, if id is smaller than the current range start,
            // it won't be in any subsequent ranges.
            if (id < r.start) break;
        }
        return false;
    }
}
