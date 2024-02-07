
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    static class Mirror {
        int[] rows;
        int[] cols;

        Mirror(int[] rows, int[] cols) {
            this.rows = rows;
            this.cols = cols;
        }
    }

    public static void main(String[] args) {
        String fileName = "input.txt";
        List<String> input = readFile(fileName);
        System.out.println(solve(input));
    }

    private static List<String> readFile(String fileName) {
        List<String> lines = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return lines;
    }

    private static List<Mirror> parseInput(List<String> input) {
        List<Mirror> mirrors = new ArrayList<>();
        List<String> mirrorStr = new ArrayList<>();

        for (String line : input) {
            if (line.equals("")) {
                mirrors.add(parseMirror(mirrorStr));
                mirrorStr.clear();
            } else {
                mirrorStr.add(line);
            }
        }
        mirrors.add(parseMirror(mirrorStr));

        return mirrors;
    }

    private static Mirror parseMirror(List<String> mirrorStr) {
        int[] rows = new int[mirrorStr.size()];
        int[] cols = new int[mirrorStr.get(0).length()];

        for (int y = 0; y < mirrorStr.size(); y++) {
            String line = mirrorStr.get(y);
            for (int x = 0; x < line.length(); x++) {
                rows[y] <<= 1;
                cols[x] <<= 1;
                if (line.charAt(x) == '#') {
                    rows[y]++;
                    cols[x]++;
                }
            }
        }

        return new Mirror(rows, cols);
    }

    private static int getMirrorAxis(int[] lines) {
        for (int i = 1; i < lines.length; i++) {
            boolean isMirror = true;

            for (int j = 0; isMirror && j < Math.min(i, lines.length - i); j++) {
                if (lines[i - 1 - j] != lines[i + j]) {
                    isMirror = false;
                }
            }

            if (isMirror) {
                return i;
            }
        }

        return 0;
    }

    private static int getMirrorAxisWithOneSmudge(int[] lines) {
        for (int i = 1; i < lines.length; i++) {
            boolean isMirror = true;
            int numSmudges = 0;

            for (int j = 0; isMirror && j < Math.min(i, lines.length - i); j++) {
                if (lines[i - 1 - j] != lines[i + j]) {
                    if (numSmudges > 0) {
                        isMirror = false;
                    } else {
                        int dif = lines[i - 1 - j] ^ lines[i + j];
                        boolean isOnlyOneSmudge = (dif & (dif - 1)) == 0;
                        if (isOnlyOneSmudge) {
                            numSmudges++;
                        } else {
                            isMirror = false;
                        }
                    }
                }
            }

            if (isMirror && numSmudges == 1) {
                return i;
            }
        }

        return 0;
    }

    private static int solve(List<String> input) {
        List<Mirror> mirrors = parseInput(input);

        int res = 0;
        for (Mirror mirror : mirrors) {
            res += getMirrorAxis(mirror.cols);
            res += getMirrorAxis(mirror.rows) * 100;
        }
        return res;
    }
}
