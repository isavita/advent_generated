
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Main {

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<String> grid = new ArrayList<>();
            StringBuilder movesBuffer = new StringBuilder();
            boolean readingMap = true;
            String line;

            while ((line = br.readLine()) != null) {
                if (readingMap) {
                    if (line.contains("#")) {
                        grid.add(line);
                    } else {
                        readingMap = false;
                        movesBuffer.append(line);
                    }
                } else {
                    movesBuffer.append(line);
                }
            }

            String moves = movesBuffer.toString();
            int rows = grid.size();
            int cols = grid.get(0).length();
            char[][] runes = new char[rows][cols];
            for (int i = 0; i < rows; i++) {
                runes[i] = grid.get(i).toCharArray();
            }

            int robotR = 0, robotC = 0;
            for (int r = 0; r < rows; r++) {
                for (int c = 0; c < cols; c++) {
                    if (runes[r][c] == '@') {
                        robotR = r;
                        robotC = c;
                        break;
                    }
                }
            }

            Map<Character, int[]> dirs = new HashMap<>();
            dirs.put('^', new int[]{-1, 0});
            dirs.put('v', new int[]{1, 0});
            dirs.put('<', new int[]{0, -1});
            dirs.put('>', new int[]{0, 1});

            for (char move : moves.toCharArray()) {
                int[] d = dirs.get(move);
                int nr = robotR + d[0];
                int nc = robotC + d[1];
                if (runes[nr][nc] == '#') {
                    continue;
                } else if (runes[nr][nc] == 'O') {
                    if (!pushBoxes(runes, nr, nc, d[0], d[1])) {
                        continue;
                    }
                }
                if (runes[nr][nc] == '.' || runes[nr][nc] == 'O') {
                    runes[robotR][robotC] = '.';
                    runes[nr][nc] = '@';
                    robotR = nr;
                    robotC = nc;
                }
            }

            int sum = 0;
            for (int r = 0; r < rows; r++) {
                for (int c = 0; c < cols; c++) {
                    if (runes[r][c] == 'O') {
                        sum += r * 100 + c;
                    }
                }
            }
            System.out.println(sum);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static boolean pushBoxes(char[][] runes, int r, int c, int dr, int dc) {
        int nr = r + dr;
        int nc = c + dc;
        if (runes[nr][nc] == '#') {
            return false;
        }
        if (runes[nr][nc] == 'O') {
            if (!pushBoxes(runes, nr, nc, dr, dc)) {
                return false;
            }
        }
        if (runes[nr][nc] == '.') {
            runes[nr][nc] = 'O';
            runes[r][c] = '.';
            return true;
        }
        return false;
    }
}
