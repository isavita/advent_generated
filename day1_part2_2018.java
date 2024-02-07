
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            HashSet<Integer> frequencies = new HashSet<>();
            int currentFrequency = 0;
            frequencies.add(currentFrequency);

            while (true) {
                while ((line = reader.readLine()) != null) {
                    int frequencyDelta = Integer.parseInt(line.trim());
                    currentFrequency += frequencyDelta;
                    if (frequencies.contains(currentFrequency)) {
                        System.out.println(currentFrequency);
                        reader.close();
                        return;
                    }
                    frequencies.add(currentFrequency);
                }
                reader.close();
                reader = new BufferedReader(new FileReader("input.txt"));
            }
        } catch (IOException e) {
            System.out.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
    }
}
