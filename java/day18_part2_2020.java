
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {

  static long evaluateSimple(String expression) {
    Matcher m = Pattern.compile("\\d+|\\+|\\*").matcher(expression);
    List<String> parts = new ArrayList<>();
    while (m.find()) {
      parts.add(m.group());
    }
    long total = Long.parseLong(parts.get(0));
    for (int i = 1; i < parts.size(); i += 2) {
      if (parts.get(i).equals("+")) {
        total += Long.parseLong(parts.get(i + 1));
      } else {
        total *= Long.parseLong(parts.get(i + 1));
      }
    }
    return total;
  }

  static long evaluateAdvanced(String expression) {
    Matcher m = Pattern.compile("\\d+|\\+|\\*").matcher(expression);
    List<String> parts = new ArrayList<>();
    while (m.find()) {
      parts.add(m.group());
    }
    while (parts.contains("+")) {
      int i = parts.indexOf("+");
      long total = Long.parseLong(parts.get(i - 1)) + Long.parseLong(parts.get(i + 1));
      parts.subList(i - 1, i + 2).clear();
      parts.add(i - 1, String.valueOf(total));
    }
    long total = Long.parseLong(parts.get(0));
    for (int i = 1; i < parts.size(); i += 2) {
      total *= Long.parseLong(parts.get(i + 1));
    }
    return total;
  }

  static long evaluateExpression(String expression, boolean advanced) {
    while (expression.contains("(")) {
      int start = expression.lastIndexOf('(');
      int end = -1;
      int openCount = 0;
      for (int i = start; i < expression.length(); i++) {
        if (expression.charAt(i) == '(') {
          openCount++;
        } else if (expression.charAt(i) == ')') {
          openCount--;
          if (openCount == 0) {
            end = i;
            break;
          }
        }
      }
      expression =
          expression.substring(0, start)
              + (advanced
                  ? evaluateAdvanced(expression.substring(start + 1, end))
                  : evaluateSimple(expression.substring(start + 1, end)))
              + expression.substring(end + 1);
    }
    return advanced ? evaluateAdvanced(expression) : evaluateSimple(expression);
  }

  public static void main(String[] args) throws IOException {
    List<String> expressions = Files.readAllLines(Paths.get("input.txt"));
    long resultPart1 = 0;
    long resultPart2 = 0;
    for (String expression : expressions) {
      resultPart1 += evaluateExpression(expression, false);
      resultPart2 += evaluateExpression(expression, true);
    }
    System.out.println(resultPart1);
    System.out.println(resultPart2);
  }
}
