
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TicketTranslation {

  static class Range {
    int start;
    int end;

    Range(int start, int end) {
      this.start = start;
      this.end = end;
    }

    boolean contains(int value) {
      return value >= start && value <= end;
    }
  }

  public static void main(String[] args) throws IOException {
    String input = new String(Files.readAllBytes(Paths.get("input.txt"))).trim();
    String[] parts = input.split("\n\n");
    String rules = parts[0];
    String nearbyTickets = parts[2];

    Map<String, List<Range>> validRanges = parseRules(rules);
    int errorRate = calculateErrorRate(validRanges, nearbyTickets);

    System.out.println(errorRate);
  }

  static Map<String, List<Range>> parseRules(String rules) {
    Map<String, List<Range>> validRanges = new HashMap<>();
    for (String rule : rules.split("\n")) {
      String[] parts = rule.split(": ");
      String field = parts[0];
      List<Range> ranges = new ArrayList<>();
      Matcher m = Pattern.compile("(\\d+)-(\\d+)").matcher(parts[1]);
      while (m.find()) {
        ranges.add(new Range(Integer.parseInt(m.group(1)), Integer.parseInt(m.group(2))));
      }
      validRanges.put(field, ranges);
    }
    return validRanges;
  }

  static int calculateErrorRate(Map<String, List<Range>> validRanges, String nearbyTickets) {
    int errorRate = 0;
    for (String ticket : nearbyTickets.split("\n")) {
      if (ticket.startsWith("nearby")) continue;
      String[] values = ticket.split(",");
      for (String valueStr : values) {
        int value = Integer.parseInt(valueStr);
        boolean valid = false;
        for (List<Range> ranges : validRanges.values()) {
          if (isValid(value, ranges)) {
            valid = true;
            break;
          }
        }
        if (!valid) {
          errorRate += value;
        }
      }
    }
    return errorRate;
  }

  static boolean isValid(int value, List<Range> ranges) {
    for (Range range : ranges) {
      if (range.contains(value)) {
        return true;
      }
    }
    return false;
  }
}
