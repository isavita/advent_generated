import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class XMAS {
    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<Long> numbers = new ArrayList<>();
            String line;
            while ((line = br.readLine()) != null) {
                numbers.add(Long.parseLong(line));
            }

            int preambleSize = 25;
            for (int i = preambleSize; i < numbers.size(); i++) {
                long target = numbers.get(i);
                boolean found = false;
                for (int j = i - preambleSize; j < i; j++) {
                    for (int k = j + 1; k < i; k++) {
                        if (numbers.get(j) + numbers.get(k) == target) {
                            found = true;
                            break;
                        }
                    }
                    if (found) {
                        break;
                    }
                }
                if (!found) {
                    System.out.println("The first number that does not have the property is: " + target);
                    return;
                }
            }
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }
}