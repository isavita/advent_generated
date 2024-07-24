
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

public class SpiralMemory {
    public static void main(String[] args) throws IOException {
        int target = Integer.parseInt(new String(Files.readAllBytes(Paths.get("input.txt"))).trim());
        Map<String, Integer> grid = new HashMap<>();
        grid.put("0,0", 1);

        int x = 0, y = 0, dx = 0, dy = -1;

        while (true) {
            if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)) {
                int temp = dx;
                dx = -dy;
                dy = temp;
            }

            x += dx;
            y += dy;

            int value = 0;
            for (int dx1 = -1; dx1 <= 1; dx1++) {
                for (int dy1 = -1; dy1 <= 1; dy1++) {
                    value += grid.getOrDefault((x + dx1) + "," + (y + dy1), 0);
                }
            }
            grid.put(x + "," + y, value);

            if (value > target) {
                System.out.println(value);
                break;
            }
        }
    }
}
