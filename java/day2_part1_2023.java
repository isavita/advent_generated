
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class solution {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            Pattern gamePattern = Pattern.compile("Game (\\d+): (.+)");
            Pattern cubePattern = Pattern.compile("(\\d+) (red|green|blue)");
            int totalSum = 0;

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                Matcher gameMatcher = gamePattern.matcher(line);

                if (gameMatcher.find()) {
                    int gameId = Integer.parseInt(gameMatcher.group(1));
                    String[] rounds = gameMatcher.group(2).split(";");

                    boolean isValid = true;

                    for (String round : rounds) {
                        Matcher cubeMatcher = cubePattern.matcher(round);
                        int red = 0, green = 0, blue = 0;

                        while (cubeMatcher.find()) {
                            int count = Integer.parseInt(cubeMatcher.group(1));
                            String color = cubeMatcher.group(2);

                            switch (color) {
                                case "red":
                                    red += count;
                                    break;
                                case "green":
                                    green += count;
                                    break;
                                case "blue":
                                    blue += count;
                                    break;
                            }

                            if (red > 12 || green > 13 || blue > 14) {
                                isValid = false;
                                break;
                            }
                        }

                        if (!isValid) {
                            break;
                        }
                    }

                    if (isValid) {
                        totalSum += gameId;
                    }
                }
            }

            scanner.close();
            System.out.println(totalSum);
        } catch (FileNotFoundException e) {
            System.out.println("Error opening file: " + e.getMessage());
        }
    }
}
