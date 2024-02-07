
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class solution {
    public static void main(String[] args) {
        try {
            String data = new String(Files.readAllBytes(Paths.get("input.txt"))).trim();
            int width = 25;
            int height = 6;
            int layerSize = width * height;

            int minZeros = layerSize + 1;
            int result = 0;

            for (int i = 0; i < data.length(); i += layerSize) {
                String layer = data.substring(i, Math.min(i + layerSize, data.length()));
                int zeroCount = 0;
                int oneCount = 0;
                int twoCount = 0;

                for (int j = 0; j < layer.length(); j++) {
                    switch (layer.charAt(j)) {
                        case '0':
                            zeroCount++;
                            break;
                        case '1':
                            oneCount++;
                            break;
                        case '2':
                            twoCount++;
                            break;
                    }
                }

                if (zeroCount < minZeros) {
                    minZeros = zeroCount;
                    result = oneCount * twoCount;
                }
            }

            System.out.println(result);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
