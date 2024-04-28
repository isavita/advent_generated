import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class TransparentOrigami {
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
        String line;

        // Read dots
        List<int[]> dots = new ArrayList<>();
        while (!(line = reader.readLine()).isEmpty()) {
            String[] parts = line.split(",");
            dots.add(new int[]{Integer.parseInt(parts[0]), Integer.parseInt(parts[1])});
        }

        // Read folds
        List<String[]> folds = new ArrayList<>();
        while ((line = reader.readLine()) != null) {
            String[] parts = line.split("=");
            folds.add(new String[]{parts[0].charAt(parts[0].length() - 1) + "", parts[1]});
        }

        // Perform first fold
        String[] firstFold = folds.get(0);
        char axis = firstFold[0].charAt(0);
        int value = Integer.parseInt(firstFold[1]);
        for (int i = 0; i < dots.size(); i++) {
            if (axis == 'x' && dots.get(i)[0] > value) {
                dots.set(i, new int[]{2 * value - dots.get(i)[0], dots.get(i)[1]});
            } else if (axis == 'y' && dots.get(i)[1] > value) {
                dots.set(i, new int[]{dots.get(i)[0], 2 * value - dots.get(i)[1]});
            }
        }

        // Count visible dots
        Set<String> visibleDots = new HashSet<>();
        for (int[] dot : dots) {
            visibleDots.add(dot[0] + "," + dot[1]);
        }
        System.out.println("Visible dots after first fold: " + visibleDots.size());
    }
}