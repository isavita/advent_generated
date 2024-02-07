
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class solution {
    private static final int[][] NEIGHBORS4 = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

    public static void main(String[] args) {
        Map<String, Integer> grid = new HashMap<>();
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            int y = 0;
            while ((line = reader.readLine()) != null) {
                char[] chars = line.toCharArray();
                for (int x = 0; x < chars.length; x++) {
                    grid.put(x + "," + y, Character.getNumericValue(chars[x]));
                }
                y++;
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        int maxScore = 0;
        for (String point : grid.keySet()) {
            int score = 1;
            String[] parts = point.split(",");
            int pX = Integer.parseInt(parts[0]);
            int pY = Integer.parseInt(parts[1]);

            for (int[] n : NEIGHBORS4) {
                int view = 0;
                int[] next = {pX, pY};
                while (true) {
                    next[0] += n[0];
                    next[1] += n[1];
                    if (grid.containsKey(next[0] + "," + next[1])) {
                        view++;
                        if (grid.get(next[0] + "," + next[1]) >= grid.get(pX + "," + pY)) {
                            score *= view;
                            break;
                        }
                    } else {
                        score *= view;
                        break;
                    }
                }
            }

            if (score > maxScore) {
                maxScore = score;
            }
        }
        System.out.println(maxScore);
    }
}
