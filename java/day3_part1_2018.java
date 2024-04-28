import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FabricClaims {

    private static final int FABRIC_SIZE = 1000;
    private static final int[][] fabric = new int[FABRIC_SIZE][FABRIC_SIZE];

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                processClaim(line);
            }
            System.out.println(countOverlaps());
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }

    private static void processClaim(String claim) {
        Pattern pattern = Pattern.compile("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)");
        Matcher matcher = pattern.matcher(claim);
        if (matcher.matches()) {
            int id = Integer.parseInt(matcher.group(1));
            int left = Integer.parseInt(matcher.group(2));
            int top = Integer.parseInt(matcher.group(3));
            int width = Integer.parseInt(matcher.group(4));
            int height = Integer.parseInt(matcher.group(5));
            for (int i = left; i < left + width; i++) {
                for (int j = top; j < top + height; j++) {
                    fabric[i][j]++;
                }
            }
        }
    }

    private static int countOverlaps() {
        int overlaps = 0;
        for (int i = 0; i < FABRIC_SIZE; i++) {
            for (int j = 0; j < FABRIC_SIZE; j++) {
                if (fabric[i][j] > 1) {
                    overlaps++;
                }
            }
        }
        return overlaps;
    }
}