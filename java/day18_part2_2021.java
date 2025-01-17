
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Snailfish {

    public static void main(String[] args) {
        List<String> lines = readInput("input.txt");

        // Part 1
        String sum = lines.get(0);
        for (int i = 1; i < lines.size(); i++) {
            sum = add(sum, lines.get(i));
        }
        System.out.println("Magnitude of the final sum: " + calculateMagnitude(sum));

        // Part 2
        int maxMagnitude = 0;
        for (int i = 0; i < lines.size(); i++) {
            for (int j = 0; j < lines.size(); j++) {
                if (i != j) {
                    int magnitude = calculateMagnitude(add(lines.get(i), lines.get(j)));
                    maxMagnitude = Math.max(maxMagnitude, magnitude);
                }
            }
        }
        System.out.println("Largest magnitude of any sum of two different snailfish numbers: " + maxMagnitude);
    }

    private static List<String> readInput(String filename) {
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return lines;
    }

    private static String add(String s1, String s2) {
        String result = "[" + s1 + "," + s2 + "]";
        return reduce(result);
    }

    private static String reduce(String s) {
        while (true) {
            String exploded = explode(s);
            if (!exploded.equals(s)) {
                s = exploded;
                continue;
            }
            String split = split(s);
            if (!split.equals(s)) {
                s = split;
                continue;
            }
            break;
        }
        return s;
    }

    private static String explode(String s) {
        int depth = 0;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '[') {
                depth++;
            } else if (c == ']') {
                depth--;
            } else if (depth > 4 && Character.isDigit(c)) {
                int j = i + 1;
                while (j < s.length() && Character.isDigit(s.charAt(j))) {
                    j++;
                }
                if (s.charAt(j) == ',') {
                    int k = j + 1;
                    while (k < s.length() && Character.isDigit(s.charAt(k))) {
                        k++;
                    }
                    if (s.charAt(k) == ']') {
                        int left = Integer.parseInt(s.substring(i, j));
                        int right = Integer.parseInt(s.substring(j + 1, k));
                        String prefix = s.substring(0, i - 1);
                        String suffix = s.substring(k + 1);
                        prefix = addToRightmost(prefix, left);
                        suffix = addToLeftmost(suffix, right);
                        return prefix + "0" + suffix;
                    }
                }
            }
        }
        return s;
    }

    private static String addToRightmost(String s, int num) {
        for (int i = s.length() - 1; i >= 0; i--) {
            if (Character.isDigit(s.charAt(i))) {
                int j = i;
                while (j >= 0 && Character.isDigit(s.charAt(j))) {
                    j--;
                }
                int n = Integer.parseInt(s.substring(j + 1, i + 1));
                return s.substring(0, j + 1) + (n + num) + s.substring(i + 1);
            }
        }
        return s;
    }

    private static String addToLeftmost(String s, int num) {
        for (int i = 0; i < s.length(); i++) {
            if (Character.isDigit(s.charAt(i))) {
                int j = i;
                while (j < s.length() && Character.isDigit(s.charAt(j))) {
                    j++;
                }
                int n = Integer.parseInt(s.substring(i, j));
                return s.substring(0, i) + (n + num) + s.substring(j);
            }
        }
        return s;
    }

    private static String split(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (Character.isDigit(s.charAt(i))) {
                int j = i + 1;
                while (j < s.length() && Character.isDigit(s.charAt(j))) {
                    j++;
                }
                int num = Integer.parseInt(s.substring(i, j));
                if (num >= 10) {
                    int left = num / 2;
                    int right = (num + 1) / 2;
                    return s.substring(0, i) + "[" + left + "," + right + "]" + s.substring(j);
                }
            }
        }
        return s;
    }

    private static int calculateMagnitude(String s) {
        while (s.contains("[")) {
            int openBracket = s.lastIndexOf('[');
            int closeBracket = s.indexOf(']', openBracket);
            String pair = s.substring(openBracket + 1, closeBracket);
            String[] parts = pair.split(",");
            int left = Integer.parseInt(parts[0]);
            int right = Integer.parseInt(parts[1]);
            int magnitude = 3 * left + 2 * right;
            s = s.substring(0, openBracket) + magnitude + s.substring(closeBracket + 1);
        }
        return Integer.parseInt(s);
    }
}
