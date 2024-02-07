
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class solution {
    public static void main(String[] args) {
        int totalPower = 0;
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            Pattern gamePattern = Pattern.compile("Game (\\d+): (.+)");
            Pattern cubePattern = Pattern.compile("(\\d+) (red|green|blue)");

            while ((line = reader.readLine()) != null) {
                Matcher gameMatcher = gamePattern.matcher(line);
                if (gameMatcher.find()) {
                    String[] rounds = gameMatcher.group(2).split(";");

                    int maxRed = 0;
                    int maxGreen = 0;
                    int maxBlue = 0;

                    for (String round : rounds) {
                        Matcher cubeMatcher = cubePattern.matcher(round);
                        int red = 0;
                        int green = 0;
                        int blue = 0;

                        while (cubeMatcher.find()) {
                            int count = Integer.parseInt(cubeMatcher.group(1));
                            switch (cubeMatcher.group(2)) {
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
                        }

                        if (red > maxRed) {
                            maxRed = red;
                        }
                        if (green > maxGreen) {
                            maxGreen = green;
                        }
                        if (blue > maxBlue) {
                            maxBlue = blue;
                        }
                    }

                    int power = maxRed * maxGreen * maxBlue;
                    totalPower += power;
                }
            }

            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

        System.out.println(totalPower);
    }
}
