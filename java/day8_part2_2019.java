
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class ImageDecoder {
    public static void main(String[] args) throws IOException {
        String imageData = new String(Files.readAllBytes(Paths.get("input.txt"))).trim();
        int width = 25, height = 6, layerSize = width * height;
        char[] finalImage = new char[layerSize];
        java.util.Arrays.fill(finalImage, '2');

        for (int i = 0; i < imageData.length(); i += layerSize) {
            String layer = imageData.substring(i, Math.min(i + layerSize, imageData.length()));
            for (int j = 0; j < layer.length(); j++) {
                if (finalImage[j] == '2') {
                    finalImage[j] = layer.charAt(j);
                }
            }
        }

        System.out.println("Decoded image:");
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                char pixel = finalImage[i * width + j];
                System.out.print(pixel == '0' ? " " : "#");
            }
            System.out.println();
        }
    }
}
