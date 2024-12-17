
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Main {

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<String> lines = new ArrayList<>();
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line.trim());
            }

            char[][] ground = new char[1][1];
            ground[0][0] = '+';

            int maxX = 0, minX = 0, maxY = 0, minY = 20;
            int xOffset = 500, yOffset = 0;

            Pattern pattern = Pattern.compile("[=, .]+");

            for (String l : lines) {
                String[] split = pattern.split(l);
                if (split[0].equals("x")) {
                    int x = Integer.parseInt(split[1]) - xOffset;
                    int y1 = Integer.parseInt(split[3]) - yOffset;
                    int y2 = Integer.parseInt(split[4]) - yOffset;

                    while (x >= maxX) {
                        maxX++;
                        for (int j = 0; j < ground.length; j++) {
                            ground[j] = append(ground[j], '.');
                        }
                    }
                    while (x <= minX) {
                        minX--;
                        for (int j = 0; j < ground.length; j++) {
                            ground[j] = prepend(ground[j], '.');
                        }
                    }
                    while (y2 > maxY) {
                        maxY++;
                        ground = append(ground, new char[ground[0].length]);
                        for (int j = 0; j < ground[ground.length - 1].length; j++) {
                            ground[ground.length - 1][j] = '.';
                        }
                    }
                    if (y1 < minY) {
                        minY = y1;
                    }
                    for (int i = y1; i <= y2; i++) {
                        ground[i][x - minX] = '#';
                    }

                } else {
                    int y = Integer.parseInt(split[1]) - yOffset;
                    int x1 = Integer.parseInt(split[3]) - xOffset;
                    int x2 = Integer.parseInt(split[4]) - xOffset;

                    while (y > maxY) {
                        maxY++;
                        ground = append(ground, new char[ground[0].length]);
                        for (int j = 0; j < ground[ground.length - 1].length; j++) {
                            ground[ground.length - 1][j] = '.';
                        }
                    }
                    while (x2 >= maxX) {
                        maxX++;
                        for (int j = 0; j < ground.length; j++) {
                            ground[j] = append(ground[j], '.');
                        }
                    }
                    while (x1 <= minX) {
                        minX--;
                        for (int j = 0; j < ground.length; j++) {
                            ground[j] = prepend(ground[j], '.');
                        }
                    }
                    for (int i = x1; i <= x2; i++) {
                        ground[y][i - minX] = '#';
                    }
                    if (y < minY) {
                        minY = y;
                    }
                }
            }

            int waterCount = 0;
            int flowCount = 0;
            int roundLimit = 200000;

            while (ground[1][-minX] != '|' && waterCount < roundLimit) {
                boolean canMove = true;
                int x = -minX;
                int y = 1;
                int tryLeft = 0;
                while (canMove) {
                    if (y + 1 > maxY || ground[y + 1][x] == '|') {
                        ground[y][x] = '|';
                        canMove = false;
                        if (y >= minY) {
                            flowCount++;
                        }
                    } else if (ground[y + 1][x] == '.') {
                        y++;
                        tryLeft = 0;
                    } else if (ground[y + 1][x] == '#' || ground[y + 1][x] == '~') {
                        if ((tryLeft == 1 && ground[y][x - 1] == '|') ||
                                (tryLeft == 2 && ground[y][x + 1] == '|') ||
                                (ground[y][x + 1] == '|' && ground[y][x - 1] != '.') ||
                                (ground[y][x + 1] != '.' && ground[y][x - 1] == '|')) {
                            ground[y][x] = '|';
                            flowCount++;
                            canMove = false;
                            for (int i = x + 1; i < ground[0].length && ground[y][i] == '~'; i++) {
                                ground[y][i] = '|';
                                waterCount--;
                                flowCount++;
                            }
                            for (int i = x - 1; i >= 0 && ground[y][i] == '~'; i--) {
                                ground[y][i] = '|';
                                waterCount--;
                                flowCount++;
                            }
                        } else if ((tryLeft == 0 && ground[y][x - 1] == '.') ||
                                (tryLeft == 1 && ground[y][x - 1] == '.')) {
                            x--;
                            tryLeft = 1;
                        } else if ((tryLeft == 0 && ground[y][x + 1] == '.') ||
                                (tryLeft == 2 && ground[y][x + 1] == '.')) {
                            x++;
                            tryLeft = 2;
                        } else {
                            canMove = false;
                            ground[y][x] = '~';
                            waterCount++;
                        }
                    }
                }
            }
            System.out.println(flowCount + waterCount);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static char[] append(char[] arr, char c) {
        char[] newArr = new char[arr.length + 1];
        System.arraycopy(arr, 0, newArr, 0, arr.length);
        newArr[arr.length] = c;
        return newArr;
    }

    private static char[] prepend(char[] arr, char c) {
        char[] newArr = new char[arr.length + 1];
        System.arraycopy(arr, 0, newArr, 1, arr.length);
        newArr[0] = c;
        return newArr;
    }

    private static char[][] append(char[][] arr, char[] newRow) {
        char[][] newArr = new char[arr.length + 1][];
        System.arraycopy(arr, 0, newArr, 0, arr.length);
        newArr[arr.length] = newRow;
        return newArr;
    }
}
