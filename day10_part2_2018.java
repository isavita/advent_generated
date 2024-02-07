
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class solution {

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            Star head = new Star();
            Star tail = head;
            Pattern pattern = Pattern.compile("position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>");
            while ((line = reader.readLine()) != null) {
                Matcher matcher = pattern.matcher(line);
                if (matcher.find()) {
                    Star star = new Star(Integer.parseInt(matcher.group(1)), Integer.parseInt(matcher.group(2)),
                            Integer.parseInt(matcher.group(3)), Integer.parseInt(matcher.group(4)));
                    tail.next = star;
                    tail = star;
                }
            }
            reader.close();

            int smallestT = 0;
            int smallestArea = Integer.MAX_VALUE;
            for (int t = 1; t < 100000; t++) {
                int maxX = 0;
                int maxY = 0;
                int minX = 0;
                int minY = 0;

                for (Star temp = head.next; temp.next != null; temp = temp.next) {
                    int x = temp.x + temp.vX * t;
                    if (maxX < x) {
                        maxX = x;
                    } else if (minX > x) {
                        minX = x;
                    }
                    int y = temp.y + temp.vY * t;
                    if (maxY < y) {
                        maxY = y;
                    } else if (minY > y) {
                        minY = y;
                    }
                }

                int lenX = maxX - minY + 1;
                int lenY = maxY - minY + 1;
                int area = lenX + lenY;

                if (smallestArea > area) {
                    smallestArea = area;
                    smallestT = t;
                }
            }
            System.out.println(smallestT);

            int t = smallestT;

            int maxX = 0;
            int maxY = 0;
            int minX = 0;
            int minY = 0;

            for (Star temp = head.next; temp.next != null; temp = temp.next) {
                temp.x = temp.x + temp.vX * t;
                if (maxX < temp.x) {
                    maxX = temp.x;
                } else if (minX > temp.x) {
                    minX = temp.x;
                }
                temp.y = temp.y + temp.vY * t;
                if (maxY < temp.y) {
                    maxY = temp.y;
                } else if (minY > temp.y) {
                    minY = temp.y;
                }
            }

            boolean[][] mapper = new boolean[maxY - minY + 1][maxX - minX + 1];

            for (int i = 0; i < mapper.length; i++) {
                for (int j = 0; j < mapper[0].length; j++) {
                }
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static class Star {
        int x;
        int y;
        int vX;
        int vY;
        Star next;

        public Star() {
        }

        public Star(int x, int y, int vX, int vY) {
            this.x = x;
            this.y = y;
            this.vX = vX;
            this.vY = vY;
            this.next = null;
        }
    }
}
