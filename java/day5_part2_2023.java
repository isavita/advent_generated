import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;

public class Main {

    static class RangeMap {
        long srcStart, destStart, length;
    }

    static long reverseConvertNumber(long number, RangeMap[] ranges) {
        for (int i = ranges.length - 1; i >= 0; i--) {
            RangeMap r = ranges[i];
            if (number >= r.destStart && number < r.destStart + r.length) {
                return r.srcStart + (number - r.destStart);
            }
        }
        return number;
    }

    static boolean isInSeedRanges(long number, long[][] ranges) {
        for (long[] r : ranges) {
            if (number >= r[0] && number < r[0] + r[1]) {
                return true;
            }
        }
        return false;
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String line;
        long[][] seedRanges = new long[0][2];
        RangeMap[] currentRanges = new RangeMap[0];
        java.util.List<RangeMap[]> maps = new java.util.ArrayList<>();

        while ((line = br.readLine()) != null) {
            if (line.contains("map:")) {
                if (currentRanges.length > 0) {
                    maps.add(currentRanges);
                    currentRanges = new RangeMap[0];
                }
            } else if (line.startsWith("seeds:")) {
                String[] seedStrs = line.substring(7).split(" ");
                seedRanges = new long[seedStrs.length / 2][2];
                for (int i = 0; i < seedStrs.length; i += 2) {
                    seedRanges[i / 2][0] = Long.parseLong(seedStrs[i]);
                    seedRanges[i / 2][1] = Long.parseLong(seedStrs[i + 1]);
                }
            } else {
                String[] numbers = line.split(" ");
                if (numbers.length == 3) {
                    RangeMap rangeMap = new RangeMap();
                    rangeMap.srcStart = Long.parseLong(numbers[1]);
                    rangeMap.destStart = Long.parseLong(numbers[0]);
                    rangeMap.length = Long.parseLong(numbers[2]);
                    currentRanges = Arrays.copyOf(currentRanges, currentRanges.length + 1);
                    currentRanges[currentRanges.length - 1] = rangeMap;
                }
            }
        }
        if (currentRanges.length > 0) {
            maps.add(currentRanges);
        }

        long location = 0;
        while (true) {
            long seed = location;
            for (int i = maps.size() - 1; i >= 0; i--) {
                RangeMap[] map = maps.get(i);
                seed = reverseConvertNumber(seed, map);
            }

            if (isInSeedRanges(seed, seedRanges)) {
                System.out.println(location);
                break;
            }
            location++;
        }
    }
}