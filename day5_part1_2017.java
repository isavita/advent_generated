
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            List<Integer> offsets = new ArrayList<>();
            String line;

            while ((line = reader.readLine()) != null) {
                offsets.add(Integer.parseInt(line));
            }

            int index = 0;
            int steps = 0;

            while (index >= 0 && index < offsets.size()) {
                int jump = offsets.get(index);
                offsets.set(index, offsets.get(index) + 1);
                index += jump;
                steps++;
            }

            System.out.println(steps);

            reader.close();
        } catch (IOException e) {
            System.out.println("File reading error" + e);
        }
    }
}
