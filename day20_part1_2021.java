
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class solution {
    public static void main(String[] args) {
        String[] input = readInput("input.txt");
        String algorithm = input[0];
        char[][] image = new char[input.length-1][input[1].length()];
        for (int i = 1; i < input.length; i++) {
            image[i-1] = input[i].toCharArray();
        }
        image = enhanceImage(image, algorithm, 2);
        System.out.println(countLitPixels(image));
    }

    public static String[] readInput(String filename) {
        try {
            File file = new File(filename);
            Scanner scanner = new Scanner(file);
            String algorithm = scanner.nextLine();
            scanner.nextLine(); // Skip the empty line

            StringBuilder sb = new StringBuilder();
            while (scanner.hasNextLine()) {
                sb.append(scanner.nextLine());
                sb.append("\n");
            }
            String[] lines = sb.toString().split("\n");

            String[] result = new String[lines.length + 1];
            result[0] = algorithm;
            for (int i = 0; i < lines.length; i++) {
                result[i+1] = lines[i];
            }
            return result;

        } catch (FileNotFoundException e) {
            e.printStackTrace();
            return new String[0];
        }
    }

    public static char[][] enhanceImage(char[][] image, String algorithm, int times) {
        for (int i = 0; i < times; i++) {
            image = applyAlgorithm(image, algorithm, i%2 == 1 && algorithm.charAt(0) == '#');
        }
        return image;
    }

    public static char[][] applyAlgorithm(char[][] image, String algorithm, boolean flip) {
        char[][] enhancedImage = new char[image.length+2][image[0].length+2];
        for (int i = 0; i < enhancedImage.length; i++) {
            for (int j = 0; j < enhancedImage[i].length; j++) {
                int index = calculateIndex(i-1, j-1, image, flip);
                enhancedImage[i][j] = algorithm.charAt(index);
            }
        }
        return enhancedImage;
    }

    public static int calculateIndex(int i, int j, char[][] image, boolean flip) {
        int index = 0;
        for (int di = -1; di <= 1; di++) {
            for (int dj = -1; dj <= 1; dj++) {
                index <<= 1;
                if (i+di >= 0 && i+di < image.length && j+dj >= 0 && j+dj < image[0].length) {
                    if (image[i+di][j+dj] == '#') {
                        index |= 1;
                    }
                } else if (flip) {
                    index |= 1;
                }
            }
        }
        return index;
    }

    public static int countLitPixels(char[][] image) {
        int count = 0;
        for (char[] row : image) {
            for (char pixel : row) {
                if (pixel == '#') {
                    count++;
                }
            }
        }
        return count;
    }
}
