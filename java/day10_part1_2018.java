
import java.io.*;
import java.util.*;
import java.util.regex.*;

public class StarMap {
    static class Star {
        int x, y, vX, vY;

        Star(int x, int y, int vX, int vY) {
            this.x = x;
            this.y = y;
            this.vX = vX;
            this.vY = vY;
        }
    }

    public static void main(String[] args) throws IOException {
        List<Star> stars = new ArrayList<>();
        String regex = "position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>";
        Pattern pattern = Pattern.compile(regex);
        
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                Matcher matcher = pattern.matcher(line);
                if (matcher.find()) {
                    stars.add(new Star(
                        Integer.parseInt(matcher.group(1)),
                        Integer.parseInt(matcher.group(2)),
                        Integer.parseInt(matcher.group(3)),
                        Integer.parseInt(matcher.group(4))
                    ));
                }
            }
        }

        int smallestT = 0, smallestArea = Integer.MAX_VALUE;
        for (int t = 1; t < 100000; t++) {
            int minX = Integer.MAX_VALUE, minY = Integer.MAX_VALUE, maxX = Integer.MIN_VALUE, maxY = Integer.MIN_VALUE;

            for (Star star : stars) {
                int x = star.x + star.vX * t;
                int y = star.y + star.vY * t;
                minX = Math.min(minX, x);
                minY = Math.min(minY, y);
                maxX = Math.max(maxX, x);
                maxY = Math.max(maxY, y);
            }

            int area = (maxX - minX + 1) + (maxY - minY + 1);
            if (area < smallestArea) {
                smallestArea = area;
                smallestT = t;
            }
        }

        int minX = Integer.MAX_VALUE, minY = Integer.MAX_VALUE, maxX = Integer.MIN_VALUE, maxY = Integer.MIN_VALUE;

        for (Star star : stars) {
            star.x += star.vX * smallestT;
            star.y += star.vY * smallestT;
            minX = Math.min(minX, star.x);
            minY = Math.min(minY, star.y);
            maxX = Math.max(maxX, star.x);
            maxY = Math.max(maxY, star.y);
        }

        char[][] mapper = new char[maxY - minY + 1][maxX - minX + 1];
        for (char[] row : mapper) Arrays.fill(row, ' ');

        for (Star star : stars) {
            mapper[star.y - minY][star.x - minX] = '#';
        }

        for (char[] row : mapper) {
            System.out.println(new String(row));
        }
    }
}
