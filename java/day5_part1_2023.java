
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Main {

  static class RangeMap {
    long srcStart;
    long destStart;
    long length;

    public RangeMap(long srcStart, long destStart, long length) {
      this.srcStart = srcStart;
      this.destStart = destStart;
      this.length = length;
    }
  }

  static long convertNumber(long number, List<RangeMap> ranges) {
    for (RangeMap r : ranges) {
      if (number >= r.srcStart && number < r.srcStart + r.length) {
        return r.destStart + (number - r.srcStart);
      }
    }
    return number;
  }

  public static void main(String[] args) throws IOException {
    List<Long> seeds = new ArrayList<>();
    List<List<RangeMap>> maps = new ArrayList<>();
    List<RangeMap> currentRanges = new ArrayList<>();
    Scanner scanner = new Scanner(new File("input.txt"));

    while (scanner.hasNextLine()) {
      String line = scanner.nextLine();
      if (line.contains("map:")) {
        if (!currentRanges.isEmpty()) {
          maps.add(currentRanges);
          currentRanges = new ArrayList<>();
        }
      } else if (line.startsWith("seeds:")) {
        String[] seedStrs = line.substring(7).split(" ");
        for (String s : seedStrs) {
          seeds.add(Long.parseLong(s));
        }
      } else {
        String[] numbers = line.split(" ");
        if (numbers.length == 3) {
          long srcStart = Long.parseLong(numbers[1]);
          long destStart = Long.parseLong(numbers[0]);
          long length = Long.parseLong(numbers[2]);
          currentRanges.add(new RangeMap(srcStart, destStart, length));
        }
      }
    }
    maps.add(currentRanges);
    scanner.close();

    long minLocation = -1;
    for (long seed : seeds) {
      long location = seed;
      for (List<RangeMap> m : maps) {
        location = convertNumber(location, m);
      }
      if (minLocation == -1 || location < minLocation) {
        minLocation = location;
      }
    }
    System.out.println(minLocation);
  }
}
