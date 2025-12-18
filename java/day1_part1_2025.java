
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class SecretEntrance {
    private static final int TOTAL_POSITIONS = 100;
    private static final int START_POS = 50;
    private static final int TARGET_POS = 0;

    public static void main(String[] args) {
        int currentPos = START_POS;
        int zeroCount = 0;

        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;

                // Extract direction and distance
                char direction = line.charAt(0);
                int distance = Integer.parseInt(line.substring(1));

                if (direction == 'L') {
                    // Left moves toward lower numbers (counter-clockwise)
                    currentPos = (currentPos - distance) % TOTAL_POSITIONS;
                } else if (direction == 'R') {
                    // Right moves toward higher numbers (clockwise)
                    currentPos = (currentPos + distance) % TOTAL_POSITIONS;
                }

                // Java's % operator can return negative values for negative inputs.
                // We normalize it to ensure currentPos is always in [0, 99].
                if (currentPos < 0) {
                    currentPos += TOTAL_POSITIONS;
                }

                // Check if the dial is pointing at 0 after the rotation
                if (currentPos == TARGET_POS) {
                    zeroCount++;
                }
            }

            // Print the final result to standard output
            System.out.println(zeroCount);

        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        } catch (NumberFormatException e) {
            System.err.println("Error parsing distance: " + e.getMessage());
        }
    }
}
