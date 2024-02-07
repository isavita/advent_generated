
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            Map<Integer, Scanner> firewall = new HashMap<>();
            String line;

            while ((line = reader.readLine()) != null) {
                String[] fields = line.split(": ");
                int depth = Integer.parseInt(fields[0]);
                int rng = Integer.parseInt(fields[1]);
                firewall.put(depth, new Scanner(rng, 0, 1));
            }

            int delay = 0;
            while (true) {
                if (passThrough(firewall, delay)) {
                    break;
                }
                delay++;
            }

            System.out.println(delay);
            reader.close();
        } catch (IOException e) {
            System.out.println("Error reading file: " + e);
        }
    }

    public static boolean passThrough(Map<Integer, Scanner> firewall, int delay) {
        for (Map.Entry<Integer, Scanner> entry : firewall.entrySet()) {
            int depth = entry.getKey();
            Scanner scanner = entry.getValue();
            if ((depth + delay) % (2 * (scanner.getRange() - 1)) == 0) {
                return false;
            }
        }
        return true;
    }
}

class Scanner {
    private int range;
    private int position;
    private int direction;

    public Scanner(int range, int position, int direction) {
        this.range = range;
        this.position = position;
        this.direction = direction;
    }

    public int getRange() {
        return range;
    }

    public int getPosition() {
        return position;
    }

    public int getDirection() {
        return direction;
    }
}
