
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

/**
 * Day 1: Secret Entrance
 * 
 * The program calculates the number of times a circular dial (0-99) hits the 0 mark
 * given a series of rotation commands.
 */
public class SecretEntrance {

    public static void main(String[] args) {
        String fileName = "input.txt";
        
        // The dial starts at 50.
        // We use a long to track the "absolute" position (total cumulative clicks).
        // Since the dial has 100 positions (0-99), any absolute position 
        // that is a multiple of 100 corresponds to the dial pointing at 0.
        long currentAbsPos = 50;
        long totalZeroHits = 0;

        try (BufferedReader reader = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;

                char direction = line.charAt(0);
                // Parse the distance following the direction character
                int distance = Integer.parseInt(line.substring(1));

                if (direction == 'R') {
                    // Moving Right (higher numbers) increases the absolute position.
                    long nextAbsPos = currentAbsPos + distance;
                    
                    // We need to count multiples of 100 in the range (currentAbsPos, nextAbsPos].
                    // The number of multiples of N in (0, X] is floor(X/N).
                    // To find multiples in a range (A, B], we use: floor(B/100) - floor(A/100).
                    totalZeroHits += Math.floorDiv(nextAbsPos, 100) - Math.floorDiv(currentAbsPos, 100);
                    currentAbsPos = nextAbsPos;
                    
                } else if (direction == 'L') {
                    // Moving Left (lower numbers) decreases the absolute position.
                    long nextAbsPos = currentAbsPos - distance;
                    
                    // We need to count multiples of 100 in the range [nextAbsPos, currentAbsPos).
                    // This is equivalent to counting multiples in the integer range [nextAbsPos, currentAbsPos - 1].
                    // The formula for multiples of N in range [A, B] is: floor(B/N) - floor((A-1)/N).
                    totalZeroHits += Math.floorDiv(currentAbsPos - 1, 100) - Math.floorDiv(nextAbsPos - 1, 100);
                    currentAbsPos = nextAbsPos;
                }
            }
            
            // Output the final count for Part Two (method 0x434C49434B)
            System.out.println(totalZeroHits);
            
        } catch (IOException e) {
            System.err.println("Error reading file 'input.txt': " + e.getMessage());
        } catch (NumberFormatException | StringIndexOutOfBoundsException e) {
            System.err.println("Error parsing input: " + e.getMessage());
        }
    }
}
