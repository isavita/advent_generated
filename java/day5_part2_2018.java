import java.io.*;
import java.util.*;

public class Main {
    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(new File("input.txt"));
        String polymer = scanner.next();

        // Part 1
        System.out.println("Part 1: " + react(polymer).length());

        // Part 2
        int minLength = Integer.MAX_VALUE;
        for (char c = 'a'; c <= 'z'; c++) {
            String newPolymer = polymer.replaceAll("(?i)" + c, "");
            int length = react(newPolymer).length();
            minLength = Math.min(minLength, length);
        }
        System.out.println("Part 2: " + minLength);
    }

    public static String react(String polymer) {
        StringBuilder result = new StringBuilder(polymer);
        boolean reacted;
        do {
            reacted = false;
            for (int i = 0; i < result.length() - 1; i++) {
                if (Math.abs(result.charAt(i) - result.charAt(i + 1)) == 32) {
                    result.delete(i, i + 2);
                    reacted = true;
                    break;
                }
            }
        } while (reacted);
        return result.toString();
    }
}