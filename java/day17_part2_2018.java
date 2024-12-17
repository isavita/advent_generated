
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

            Pattern pattern = Pattern.compile("([xy])=(\\d+), [xy]=(\\d+)..(\\d+)");

            for (String l : lines) {
                Matcher matcher = pattern.matcher(l);
                if (matcher.find()) {
                    String type = matcher.group(1);
                    int a = Integer.parseInt(matcher.group(2)) - (type.equals("x") ? xOffset : yOffset);
                    int b = Integer.parseInt(matcher.group(3)) - (type.equals("x") ? yOffset : xOffset);
                    int c = Integer.parseInt(matcher.group(4)) - (type.equals("x") ? yOffset : xOffset);

                    if (type.equals("x")) {
                        int x = a;
                        int y1 = b;
                        int y2 = c;

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
                        minY = Math.min(minY, y1);
                        for (int i = y1; i <= y2; i++) {
                            ground[i][x - minX] = '#';
                        }
                    } else {
                        int y = a;
                        int x1 = b;
                        int x2 = c;

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
                        minY = Math.min(minY, y);
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
            System.out.println(waterCount);

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
