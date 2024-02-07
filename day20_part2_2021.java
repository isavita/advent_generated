
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static final int iterations = 50;
    public static final int expandBy = 1;

    public static void main(String[] args) {
        try {
            String[] input = readInput("input.txt");
            String algorithm = input[0];
            boolean[][] image = convertToBooleanArray(input);

            for (int i = 0; i < iterations; i++) {
                image = enhanceImage(algorithm, image, i % 2 == 1 && algorithm.charAt(0) == '#');
            }
            System.out.println(countLitPixels(image));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static String[] readInput(String filename) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(filename));
        String[] input = new String[2];

        input[0] = reader.readLine();
        reader.readLine(); // skip the empty line

        StringBuilder imageBuilder = new StringBuilder();
        String line;
        while ((line = reader.readLine()) != null) {
            imageBuilder.append(line.trim());
            imageBuilder.append("\n");
        }
        input[1] = imageBuilder.toString();

        reader.close();
        return input;
    }

    public static boolean[][] convertToBooleanArray(String[] input) {
        String[] lines = input[1].split("\n");
        boolean[][] image = new boolean[lines.length][lines[0].length()];

        for (int i = 0; i < lines.length; i++) {
            for (int j = 0; j < lines[i].length(); j++) {
                image[i][j] = lines[i].charAt(j) == '#';
            }
        }
        return image;
    }

    public static boolean[][] enhanceImage(String algorithm, boolean[][] image, boolean useInfiniteLit) {
        int sizeY = image.length + (expandBy * 2);
        int sizeX = image[0].length + (expandBy * 2);
        boolean[][] newImage = new boolean[sizeY][sizeX];

        for (int y = -expandBy; y < image.length + expandBy; y++) {
            for (int x = -expandBy; x < image[0].length + expandBy; x++) {
                int index = 0;
                for (int dy = -1; dy <= 1; dy++) {
                    for (int dx = -1; dx <= 1; dx++) {
                        index <<= 1;
                        int ny = y + dy;
                        int nx = x + dx;
                        if (ny >= 0 && ny < image.length && nx >= 0 && nx < image[0].length) {
                            if (image[ny][nx]) {
                                index |= 1;
                            }
                        } else if (useInfiniteLit) {
                            index |= 1;
                        }
                    }
                }
                newImage[y + expandBy][x + expandBy] = algorithm.charAt(index) == '#';
            }
        }
        return newImage;
    }

    public static int countLitPixels(boolean[][] image) {
        int count = 0;
        for (boolean[] row : image) {
            for (boolean pixel : row) {
                if (pixel) {
                    count++;
                }
            }
        }
        return count;
    }
}
