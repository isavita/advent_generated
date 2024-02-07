
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            StringBuilder forestBuilder = new StringBuilder();
            while ((line = reader.readLine()) != null) {
                forestBuilder.append(line).append("\n");
            }
            String[] forest = forestBuilder.toString().split("\n");
            int trees = countTrees(forest, 3, 1);
            System.out.println(trees);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static int countTrees(String[] forest, int right, int down) {
        int trees = 0;
        int x = 0;
        int width = forest[0].length();

        for (int y = 0; y < forest.length; y += down) {
            if (forest[y].charAt(x % width) == '#') {
                trees++;
            }
            x += right;
        }

        return trees;
    }
}
