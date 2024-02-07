
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class solution {
    static class Scanner {
        int range;
        int position;
        int direction;
        
        public Scanner(int range, int position, int direction) {
            this.range = range;
            this.position = position;
            this.direction = direction;
        }
    }
    
    public static void main(String[] args) {
        Map<Integer, Scanner> firewall = new HashMap<>();
        
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                String[] fields = line.split(": ");
                int depth = Integer.parseInt(fields[0]);
                int range = Integer.parseInt(fields[1]);
                firewall.put(depth, new Scanner(range, 0, 1));
            }
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
            return;
        }
        
        int severity = 0;
        
        for (int depth = 0; depth <= maxDepth(firewall); depth++) {
            if (firewall.containsKey(depth)) {
                Scanner scanner = firewall.get(depth);
                if (scanner.position == 0) {
                    severity += depth * scanner.range;
                }
            }
            
            for (Scanner scanner : firewall.values()) {
                moveScanner(scanner);
            }
        }
        
        System.out.println(severity);
    }
    
    public static int maxDepth(Map<Integer, Scanner> firewall) {
        int max = 0;
        for (int depth : firewall.keySet()) {
            if (depth > max) {
                max = depth;
            }
        }
        return max;
    }
    
    public static void moveScanner(Scanner scanner) {
        if (scanner.position == 0) {
            scanner.direction = 1;
        } else if (scanner.position == scanner.range - 1) {
            scanner.direction = -1;
        }
        scanner.position += scanner.direction;
    }
}
