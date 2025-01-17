
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class PointOfIncidence {

    public static void main(String[] args) {
        List<List<String>> patterns = readPatterns("input.txt");
        long sum = 0;
        long sumWithSmudge = 0;

        for (List<String> pattern : patterns) {
            sum += summarizePattern(pattern, -1);
            sumWithSmudge += summarizePatternWithSmudge(pattern);
        }

        System.out.println(sum);
        System.out.println(sumWithSmudge);
    }

    private static List<List<String>> readPatterns(String filename) {
        List<List<String>> patterns = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            List<String> currentPattern = new ArrayList<>();
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    if (!currentPattern.isEmpty()) {
                        patterns.add(currentPattern);
                        currentPattern = new ArrayList<>();
                    }
                } else {
                    currentPattern.add(line);
                }
            }
            if (!currentPattern.isEmpty()) {
                patterns.add(currentPattern);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return patterns;
    }

    private static long summarizePattern(List<String> pattern, int ignore) {
        int rows = pattern.size();
        int cols = pattern.get(0).length();

        for (int c = 0; c < cols - 1; c++) {
            if (c + 1 == ignore) continue;
            boolean reflects = true;
            for (int i = 0; c - i >= 0 && c + 1 + i < cols; i++) {
                for (int r = 0; r < rows; r++) {
                    if (pattern.get(r).charAt(c - i) != pattern.get(r).charAt(c + 1 + i)) {
                        reflects = false;
                        break;
                    }
                }
                if (!reflects) break;
            }
            if (reflects) return c + 1;
        }

        for (int r = 0; r < rows - 1; r++) {
            if ((r + 1) * 100 == ignore) continue;
            boolean reflects = true;
            for (int i = 0; r - i >= 0 && r + 1 + i < rows; i++) {
                if (!pattern.get(r - i).equals(pattern.get(r + 1 + i))) {
                    reflects = false;
                    break;
                }
            }
            if (reflects) return (r + 1) * 100;
        }

        return 0;
    }
    
    private static long summarizePatternWithSmudge(List<String> pattern) {
        long original = summarizePattern(pattern, -1);
        for (int r = 0; r < pattern.size(); r++) {
            for (int c = 0; c < pattern.get(0).length(); c++) {
                List<String> copy = new ArrayList<>();
                for(String row : pattern) copy.add(row);
                
                StringBuilder sb = new StringBuilder(copy.get(r));
                sb.setCharAt(c, sb.charAt(c) == '.' ? '#' : '.');
                copy.set(r, sb.toString());
                
                long summary = summarizePattern(copy, (int)original);
                if (summary > 0 && summary != original) return summary;
            }
        }
        return 0;
    }
}
